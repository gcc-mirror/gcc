#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstring>

#include <unistd.h>

#include <algorithm>
#include <fstream>
#include <array>
#include <map>
#include <set>
#include <string>
#include <vector>

#ifdef COBOL_PICTURE
const char * symbol_currency( char sign ) {
  static const char mock_signs[] = "@ŁY&";
  return strchr(mock_signs, sign);
}
void cbl_internal_error(const char *format_string, ...) { assert(false); }
#else
void cbl_internal_error(const char *format_string, ...);
const char * symbol_currency( char sign );
#endif

namespace picture_validation {

  /*
   * The picture is parsed via a state machine, implementing an automaton.  If
   * it reaches the end of the input, by definition it accepts the string.  The
   * acceptance state is a recognized type of COBOL data item.
   */
  enum state_t {
    invalid_e,
    none_e, 
    boolean_e,
    alphabetic_e, 
    unicode_e,
    alphanumeric_e, 
    alpha_ed_e,
    national_e,
    national_ed_e,
    numeric_e,
    numeric_ed_e,
    c_need_r_e,
    crdb_e, 
    d_need_b_e,
    postdec_e,
    antedec_e,
    exponent_e,
    accept_e, // keep last
  };

  const char *state_str( state_t state ) {
    switch(state) {
    case accept_e: return "accept_e";
    case alpha_ed_e: return "alpha_ed_e";
    case alphabetic_e: return "alphabetic_e";
    case alphanumeric_e: return "alphanumeric_e";
    case antedec_e: return "antedec_e";
    case boolean_e: return "boolean_e";
    case c_need_r_e: return "c_need_r_e";
    case crdb_e:  return "crdb_e";
    case d_need_b_e: return "d_need_b_e";
    case exponent_e: return "exponent_e";
    case invalid_e: return "invalid_e";
    case national_e: return "national_e";
    case national_ed_e: return "national_ed_e";
    case none_e: return "none_e";
    case numeric_e: return "numeric_e";
    case numeric_ed_e: return "numeric_ed_e";
    case postdec_e: return "postdec_e";
    case unicode_e: return "unicode_e";
    }
    return "???";
  }

  struct follow_key_t {
    state_t state;
    char ch;
    bool operator< (const follow_key_t& that ) const {
      if( state == that.state ) {
        return ch < that.ch;
      }
      return state < that.state;
    }
  };

  /*
   * The followers table enforces the merest of syntax: in a picture string,
   * what character may follow another?  It reproduces ISO table 10 in
   * programmatic form.  
   * 
   * "The symbol '+' that appears in a column and in a row by itself,
   *  represents its use in the exponent part of character-string-1 for a
   *  floating-point numeric-edited item.  
   *
   *  The symbols '+' and '-' when used as a non-floating insertion symbol
   *  appear in two columns and two rows. The leftmost column and the uppermost
   *  row for these symbols represent their use as the first symbol in
   *  character-string-1. The rightmost column and the lowermost row for these
   *  symbols represent its use as the last or penultimate symbol in
   *  character-string-1.  
   *
   *  The symbol '+' that appears in a column and in a row by itself,
   *  represents its use in the exponent."
   */

#define dot '.'
#define com ','
  typedef std::array<char, 64> follow_t;
  static const std::map <follow_key_t, follow_t> followers {
    /* all   */ { { none_e,       '\0' }, {""  "*+,-./$019ABENPSVXZCD"} },
    /*     1 */ { { antedec_e,     '1' }, {"(" "1"} },
    /*   B0/ */ { { antedec_e,     'B' }, {""  "B0/,.Z*+-$9AXVPNECD"} },
    /*   B0/ */ { { postdec_e,     'B' }, {""  "B0/,.Z*+-$9AXVPNECD"} }, // ante & post
    /*   B0/ */ { { antedec_e,     '0' }, {""  "B0/,.Z*+-$9AXVPNECD"} },
    /*   B0/ */ { { postdec_e,     '0' }, {""  "B0/,.Z*+-$9AXVPNECD"} }, // ante & post
    /*   B0/ */ { { antedec_e,     '/' }, {""  "B0/,.Z*+-$9AXVPNECD"} },
    /*   B0/ */ { { postdec_e,     '/' }, {""  "B0/,.Z*+-$9AXVPNECD"} }, // ante & post
    /*     + */ { { exponent_e,    '+' }, {""  "9"} },
    /*     , */ { { antedec_e,     com }, {""  "B0/,.Z*+-$9VPE"} },
    /*     . */ { { antedec_e,     dot }, {""  "B0/,Z*+-$9E"} }, 
    /*     . */ { { postdec_e,     dot }, {""  "B0/,Z*+-$9E"} },  // ante & post
    /*    +- */ { { antedec_e,     '+' }, {""  "B0/,.+-Z*$9VPE"} },
    /*    +- */ { { antedec_e,     '-' }, {""  "B0/,.+-Z*$9VPE"} },
    /*    Z* */ { { antedec_e,     'Z' }, {"(" "B0/,.+-Z*$9VP"} },
    /*    Z* */ { { postdec_e,     'Z' }, {""  "B0/,+-Z*"} },
    /*    Z* */ { { antedec_e,     '*' }, {""  "B0/,.+Z*$9VPCD"} },
    /*    Z* */ { { postdec_e,     '*' }, {""  "B0/,+Z*CD"} },
    /*     + */ { { antedec_e,     '+' }, {""  "B0/,.+-$9VP"} },
    /*     + */ { { postdec_e,     '+' }, {""  "B0/,+-"} },
    /*    cs */ { { antedec_e,     '$' }, {""  "B0/Z*,.+$9VP"} },
    /*    cs */ { { postdec_e,     '$' }, {""  "B0/Z*,+"} },
    /*     9 */ { { antedec_e,     '9' }, {"(" "B0/,.+-$9AXVPECD"} },
    /*     9 */ { { postdec_e,     '9' }, {"(" "B0/,.+-$9AXVPECD"} }, // ante & post
    /*    AX */ { { antedec_e,     'A' }, {"(" "B0/$9AX"} },
    /*    AX */ { { antedec_e,     'X' }, {"(" "B0/$9AX"} },
    /*     S */ { { antedec_e,     'S' }, {""  "9VP"} },
    /*     V */ { { postdec_e,     'V' }, {""  "B0/,+Z*+-$9P"} },
    /*     P */ { { antedec_e,     'P' }, {"(" "+V9P"} },
    /*     P */ { { postdec_e,     'P' }, {"(" "B0/,+Z*$P"} },
    /*     1 */ { { antedec_e,     '1' }, {""  "1"} },
    /*     N */ { { antedec_e,     'N' }, {"(" "B0/N"} },
    /*     E */ { { antedec_e,     'E' }, {""  "+9"} },

    /* CR/DB */ { { antedec_e,     'C' }, {""  "R"} },
    /* CR/DB */ { { antedec_e,     'D' }, {""  "B"} },
    /* CR/DB */ { { postdec_e,     'C' }, {""  "R"} },
    /* CR/DB */ { { postdec_e,     'D' }, {""  "B"} },
  };
  
  /*
   * As the picture string is validated, constraint_t captures what has been
   * seen: the preceding character and what characters have appeared that
   * signify alphanumeric-edited or numeric-edited.
   */
  static constexpr uint64_t hiword(char ch) { 
    return (1ULL << (ch - 'A')) << 32; 
  }
  class constraint_t {
    // Domain of symbols in a picture string.
    enum picsym_t : uint64_t {
      p_plus  = 0x0010,    // +
      p_minus = 0x0020,    // -
      p_dot   = 0x0040,    // .
      p_comma = 0x0080,    // ,
      p_slash = 0x0100,    // /
      p_cs    = 0x0200,    // $
      p_star  = 0x0400,    // *
      p_zero  = 0x0800,    // 0
      p_one   = 0x1000,    // 1
      p_nine  = 0x0009,    // 9, claim the whole nybble
      // Bits 0-15:  punctuation, currency, and 0, 1, and 9.
      // Bits 32-58: A-Z
      p_A     = hiword('A'), 
      p_B     = hiword('B'), 
      p_C     = hiword('C'), 
      p_D     = hiword('D'), 
      p_E     = hiword('E'), 
      p_N     = hiword('N'), 
      p_P     = hiword('P'), 
      p_S     = hiword('S'), 
      p_U     = hiword('U'), 
      p_V     = hiword('V'), 
      p_X     = hiword('X'), 
      p_Z     = hiword('Z'), 

      p_boolean_e = p_one, 
      p_alphabetic_e = p_A, 
      p_alphanumeric_e = (p_A | p_X | p_nine),
      p_b_0_slash_e = (p_B | p_zero | p_slash),
      p_alpha_ed_e = (p_alphanumeric_e | p_b_0_slash_e),
      p_national_e = p_N,
      p_national_ed_e = (p_N | p_b_0_slash_e),
      p_numeric_e = (p_nine | p_P | p_S | p_V),
      // B P V Z 9 0 / , . + - CR DB * cs (CR/DB not in mask because at end)
      p_numeric_ed2_e = (p_b_0_slash_e | p_comma | p_dot | p_plus | p_minus), 
      p_numeric_ed_e = ( p_P | p_V | p_Z | p_nine | p_b_0_slash_e | 
                       p_comma | p_dot | p_plus | p_minus | p_star | p_cs),
    };
    uint64_t symbol_mask;
    
    bool is_boolean() const {
      return 0 == (symbol_mask & ~p_boolean_e)
        &&   0 != (symbol_mask &  p_boolean_e);
    }
    bool is_alphabetic() const {
      return 0 == (symbol_mask & ~p_alphabetic_e)
        &&   0 != (symbol_mask &  p_alphabetic_e);
    }
    bool is_alphanumeric() const {
      return 0 == (symbol_mask & ~p_alphanumeric_e)
        &&   0 != (symbol_mask &  p_alphanumeric_e);
    }
    bool is_alpha_ed() const {
      return 0 == (symbol_mask & ~p_alpha_ed_e)
        &&   0 != (symbol_mask & p_alphanumeric_e)
        &&   0 != (symbol_mask & p_b_0_slash_e);
    }
    bool is_national() const {
      return 0 == (symbol_mask & ~p_national_e)
        &&   0 != (symbol_mask &  p_national_e);
    }
    bool is_national_ed() const {
      return 0 == (symbol_mask & ~p_national_ed_e)
        &&   0 != (symbol_mask & p_national_e)
        &&   0 != (symbol_mask & p_b_0_slash_e);
    }
    bool is_numeric() const {
      return 0 == (symbol_mask & ~p_numeric_e)
        &&   0 != (symbol_mask &  p_nine)
        &&   0 != (symbol_mask &  p_numeric_e);
    }
    bool is_numeric9_ed() const {
      return 0 == (symbol_mask & ~p_numeric_ed_e)
        &&   0 != (symbol_mask & p_nine)
        &&   0 != (symbol_mask & p_numeric_ed2_e);
    }
    bool is_numeric_ed() const {
      return 0 == (symbol_mask & ~p_numeric_ed_e)
        && ( 0 != (symbol_mask & (p_Z | p_star))
             || two_signs()
             || is_numeric9_ed() );
    }

    state_t decimal_state; // to lookup sequence validity

    uint64_t picsym_set( char ch ) {
      switch(ch) {
      case '+': return symbol_mask |= p_plus;
      case '-': return symbol_mask |= p_minus;
      case '.': return symbol_mask |= p_dot;
      case ',': return symbol_mask |= p_comma;
      case '/': return symbol_mask |= p_slash;
      case '$': return symbol_mask |= p_cs;
      case '0': return symbol_mask |= p_zero;
      case '1': return symbol_mask |= p_one;
      case '9': return symbol_mask |= p_nine;
      case 'A': return symbol_mask |= p_A;
      case 'B': return symbol_mask |= p_B;
      case 'C': return symbol_mask |= p_C;
      case 'D': return symbol_mask |= p_D;
      case 'E': return symbol_mask |= p_E;
      case 'N': return symbol_mask |= p_N;
      case 'P': return symbol_mask |= p_P;
      case 'S': return symbol_mask |= p_S;
      case 'U': return symbol_mask |= p_U;
      case 'V': return symbol_mask |= p_V;
      case 'X': return symbol_mask |= p_X;
      case 'Z': return symbol_mask |= p_Z;
      }
      return symbol_mask;
    }

    bool picsym_seen( char ch ) const {
      switch(ch) {
      case '+': return 0 < (symbol_mask & p_plus);
      case '-': return 0 < (symbol_mask & p_minus);
      case '.': return 0 < (symbol_mask & p_dot);
      case ',': return 0 < (symbol_mask & p_comma);
      case '/': return 0 < (symbol_mask & p_slash);
      case '$': return 0 < (symbol_mask & p_cs);
      case '0': return 0 < (symbol_mask & p_zero);
      case '1': return 0 < (symbol_mask & p_one);
      case '9': return 0 < (symbol_mask & p_nine);
      case 'A': return 0 < (symbol_mask & p_A);
      case 'B': return 0 < (symbol_mask & p_B);
      case 'C': return 0 < (symbol_mask & p_C);
      case 'D': return 0 < (symbol_mask & p_D);
      case 'E': return 0 < (symbol_mask & p_E);
      case 'N': return 0 < (symbol_mask & p_N);
      case 'P': return 0 < (symbol_mask & p_P);
      case 'S': return 0 < (symbol_mask & p_S);
      case 'U': return 0 < (symbol_mask & p_U);
      case 'V': return 0 < (symbol_mask & p_V);
      case 'X': return 0 < (symbol_mask & p_X);
      case 'Z': return 0 < (symbol_mask & p_Z);
      }
    }

    /*
     * 1) Every character except V adds to the capacity, with the caveat that
     *    the first CURRENCY PICTURE character adds the length of the CURRENCY
     *    SIGN string.
     * 2) data.digits has to be the count of 9 + count of Z + count of asterisk
     *    + (count of $ minus 1) + (count of - minus one) + (count of + minus 1)
     * 3) data.rdigits is the subset of 2 that is to the right of either V or
     *    . (edited) 
     * 
     * For the moment, break out the fact that V should add zero.  Right now
     * all of the regression tests assume that V, if there, adds one.  I would
     * really rather attack the V change when everything is working.
     */

    char prior_ch;
   public:
    int nplus, nminus;

    constraint_t()
      : symbol_mask(0)
      , decimal_state(none_e)
      , prior_ch('\0')
      , nplus(0), nminus(0)
    {}
    constraint_t& operator=( char ch ) {
      picsym_set(ch);
      prior_ch = ch;
      if( ch == '+' ) nplus++;
      if( ch == '-' ) nminus++;
      observe_the_dot(ch);
      return *this;
    }
    char prior() const { return prior_ch; }

    state_t automaton(state_t state) { return decimal_state = state; }
    state_t automaton() const { return decimal_state; }


    state_t state() const {
      if( is_alphabetic() )   return alphabetic_e;
      if( is_alphanumeric() ) return alphanumeric_e;
      if( is_alpha_ed() )     return alpha_ed_e;
      if( is_national() )     return national_e;
      if( is_national_ed() )  return national_ed_e;
      if( is_numeric() )      return numeric_e;
      if( is_numeric_ed() )   return numeric_ed_e;
      return invalid_e;
    }

    bool two_signs() const { return 1 < nplus || 1 < nminus; }

    /*
     * The followers table is sensitive to whether we've seen decimal point, and
     * whether we're in an exponent.  That reflects the duplicate column
     * headings in ISO Table 10.
     */
    bool day_follows_night( char ch ) {
      auto p = followers.find( follow_key_t {decimal_state, prior_ch} );
      if( p != followers.end() ) {
        const auto& candidates(p->second);
        auto pnext = std::find( candidates.begin(), candidates.end(), ch );
        return pnext != candidates.end();
      }
      return false;    
    }
  protected:
    void observe_the_dot( char ch ) {
      if( decimal_state == none_e ) decimal_state = antedec_e;
      if( ch == '.' || ch == 'V' ) decimal_state = postdec_e;    }
  } constraint;

  ////////////////////////////////////////////////////////////////

  struct transition_t {
    state_t state;
    char ch;
    state_t next_state;

    transition_t( state_t state,
                  char ch,
                  state_t next_state )
      : state(state)
      , ch(ch)
      , next_state(next_state)
    {}

    bool operator<(const transition_t& that) const {
      if (state == that.state) return ch < that.ch;      
      return state < that.state;
    }
    bool operator==(const transition_t& that) const { return   match(that); }
    bool operator!=(const transition_t& that) const { return ! match(that); }

   protected: 
    // match verifies that the row returned by std::lower_bound actually
    // matches, and isn't the next "bigger" one.
    bool match(const transition_t& that) const {
      return state == that.state
        &&   ch == that.ch;
    }
  };
  
  static std::vector<transition_t> picture_rules {
    // Alpha and Alphanumeric
    { none_e,          'A',  alphabetic_e },
    { none_e,          'X',  alphanumeric_e },
    { none_e,          'B',  alpha_ed_e },
    { none_e,          '0',  alpha_ed_e },
    { none_e,          '/',  alpha_ed_e },
    { none_e,          '9',  antedec_e },   // 9 before V
    { none_e,          'N',  national_e },
    { none_e,          'P',  none_e },
    { none_e,          'S',  none_e },
    { none_e,          'V',  postdec_e, },  // V before 9
                       
    { none_e,          '1',  antedec_e },
    { antedec_e,       '1',  antedec_e },

    { alphabetic_e,    'A',  alphabetic_e },
    { alphabetic_e,    'B',  alpha_ed_e },
    { alphabetic_e,    '0',  alpha_ed_e },
    { alphabetic_e,    '/',  alpha_ed_e },
    { alphabetic_e,    'X',  alphanumeric_e },
    { alphabetic_e,    '$',  numeric_ed_e },

    { alphanumeric_e,  'A',  alphanumeric_e },
    { alphanumeric_e,  'X',  alphanumeric_e },
    { alphanumeric_e,  '9',  alphanumeric_e },

    // Alpha edited
    { alpha_ed_e,      'B',  alpha_ed_e },
    { alpha_ed_e,      '0',  alpha_ed_e },
    { alpha_ed_e,      '/',  alpha_ed_e },
    { alpha_ed_e,      'A',  alpha_ed_e },
    { alpha_ed_e,      'X',  alpha_ed_e },
    { alpha_ed_e,      '9',  numeric_ed_e },
    { alpha_ed_e,      'Z',  numeric_ed_e },
    { alpha_ed_e,      '*',  numeric_ed_e },
    { alpha_ed_e,      '+',  numeric_ed_e },
    { alpha_ed_e,      '-',  numeric_ed_e },
    { alpha_ed_e,      dot,  numeric_ed_e },
    { alpha_ed_e,      com,  numeric_ed_e },
    { alpha_ed_e,      '$',  numeric_ed_e },

    { alphanumeric_e,  'B',  alpha_ed_e },
    { alphanumeric_e,  '0',  alpha_ed_e },
    { alphanumeric_e,  '/',  alpha_ed_e },
    
    // National
    { national_e,      'N',  national_e },
    { national_e,      '\0', alphanumeric_e },

    // National edited
    { national_e,      'B',  national_ed_e },
    { national_e,      '0',  national_ed_e },
    { national_e,      '/',  national_ed_e },

    { national_ed_e,   'N',  national_ed_e },
    { national_ed_e,   'B',  national_ed_e },
    { national_ed_e,   '0',  national_ed_e },
    { national_ed_e,   '/',  national_ed_e },
    { national_ed_e,   '\0', alpha_ed_e },

    // Numeric edited
    // [B*] or [+-].*[+-] or (9 and [$B0/,.+-], where $ is cs.
    { none_e,          'Z',  numeric_ed_e },
    { none_e,          '*',  numeric_ed_e },
    { none_e,          dot,  numeric_ed_e },
    { none_e,          com,  numeric_ed_e },
    { none_e,          '-',  numeric_ed_e },
    { none_e,          '+',  numeric_ed_e },
    { none_e,          '$',  numeric_ed_e },
                       
    { antedec_e,       '9',  antedec_e },
    { antedec_e,       'A',  alphanumeric_e },
    { antedec_e,       'P',  numeric_e },
    { antedec_e,       'V',  numeric_e },
    { antedec_e,       'X',  alphanumeric_e },
    { antedec_e,       'C',  c_need_r_e },
    { antedec_e,       'D',  d_need_b_e },

    { antedec_e,       'Z',  numeric_ed_e },
    { antedec_e,       '*',  numeric_ed_e },
    { antedec_e,       '$',  numeric_ed_e },
    { antedec_e,       dot,  numeric_ed_e },
    { antedec_e,       com,  numeric_ed_e },
                       
    { postdec_e,       '9',  numeric_e },
    { postdec_e,       'P',  numeric_e },
                       
    { numeric_e,       '9',  numeric_e },
    { numeric_e,       'P',  numeric_e },

    { numeric_e,       'Z',  numeric_ed_e },
    { numeric_e,       '*',  numeric_ed_e },
    { numeric_e,       'B',  numeric_ed_e },
    { numeric_e,       '0',  numeric_ed_e },
    { numeric_e,       '/',  numeric_ed_e },
    { numeric_e,       dot,  numeric_ed_e },
    { numeric_e,       com,  numeric_ed_e },
    { numeric_e,       '+',  numeric_ed_e },
    { numeric_e,       '-',  numeric_ed_e },
    { numeric_e,       '$',  numeric_ed_e },
    { numeric_e,       'C',  c_need_r_e },
    { numeric_e,       'D',  d_need_b_e },

    { numeric_ed_e,    'Z',  numeric_ed_e },
    { numeric_ed_e,    'B',  numeric_ed_e },
    { numeric_ed_e,    '9',  numeric_ed_e },
    { numeric_ed_e,    '0',  numeric_ed_e },
    { numeric_ed_e,    dot,  numeric_ed_e },
    { numeric_ed_e,    com,  numeric_ed_e },
    { numeric_ed_e,    '/',  numeric_ed_e },
    { numeric_ed_e,    '-',  numeric_ed_e },
    { numeric_ed_e,    '+',  numeric_ed_e },
    { numeric_ed_e,    '*',  numeric_ed_e },
    { numeric_ed_e,    '$',  numeric_ed_e },
    { numeric_ed_e,    'P',  numeric_ed_e },
    { numeric_ed_e,    'V',  numeric_ed_e }, // See Table 10 if in doubt. 
    { numeric_ed_e,    'C',  c_need_r_e },
    { numeric_ed_e,    'D',  d_need_b_e },

    // End of the road
    { c_need_r_e,      'R',  numeric_ed_e },
    { d_need_b_e,      'B',  numeric_ed_e },
  };
  
  typedef std::array<unsigned char, 256> automaton_elem_t;
  typedef std::vector<automaton_elem_t> automaton_matrix_t;

  const automaton_matrix_t& prepare_automaton() {
    static automaton_matrix_t matrix(accept_e);
    static bool ready = false;
    if( ready ) return matrix;

    int i __attribute__ ((unused)) = 0, nextra = 0;
    for( const auto& rule : picture_rules ) {
      unsigned char ch(rule.ch);

#ifdef COBOL_PICTURE
      if( matrix[rule.state][ch] ) {
        nextra++;
        fprintf(stderr, "%s: redundant rule: #%d { %s '%c' %s }\n",
                __func__, i, 
                state_str(rule.state), rule.ch, 
                state_str(rule.next_state));
      }
#endif
      matrix[rule.state][ch] = rule.next_state;
      i++;
    }
    if( nextra ) {
      cbl_internal_error("%s: %d redundant rules", __func__, nextra);
    }
    
    ready = true;
    return matrix;
  }
  
  void sort_rules() {
#ifdef COBOL_PICTURE
    // During development, verify the ruleset is unique. 
    std::set<transition_t> U;
    int i=0;
    for( const auto& r : picture_validation::picture_rules ) {
      auto result = U.insert(r);
      if( ! result.second ) {
        auto extra = result.first;
        fprintf(stderr, "%s: redundant rule: #%d { %s '%c' %s }\n",
                __func__, i, 
                state_str(extra->state),
                extra->ch, 
                state_str(extra->next_state));
      }
      i++;
    }
#endif
    std::sort( picture_validation::picture_rules.begin(),
               picture_validation::picture_rules.end() );
    auto extra = std::unique( picture_validation::picture_rules.begin(),
                              picture_validation::picture_rules.end() );
    if( extra != picture_validation::picture_rules.end() ) {
#ifndef COBOL_PICTURE
      cbl_internal_error("%s: redundant rule: { %s %qc %s }", __func__, 
                         state_str(extra->state),
                         extra->ch, 
                         state_str(extra->next_state));
#endif
      assert(false && "Redundant rules detected"); 
    }
  }
}

using picture_validation::transition_t;
using picture_validation::state_t;

/*
 * If a character within parentheses cannot be part of a COBOL word, then the
 * sequence is not an integer or data-item name.
 */
static const char *
seek_paren( const char *p, const char *epicture ) {
  for( p++; p < epicture; p++ ) {
    if( ! isalnum(*p) ) {
      switch(*p) {
      case '-':
      case '_':
        continue; // part of COBOL text word, or a digit
      }
      break;
    }
  }
  return p;
}

static bool verbose = false;

// Dot as last character is period separator because the lexer defines
// end-of-picture as a space.
bool
end_of_picture( const char *p, const char *epicture ) {
  if( p + 1 == epicture ) {
    switch( *p ) {
    case '.': case ';': case ',':
      return true;
    }
  }
  return p == epicture;
}

std::pair<const char *, state_t>
is_valid_picture(const char picture[]) {
  static bool sorted = false;
  if( ! sorted ) {
    picture_validation::sort_rules();
    sorted = true;
  }
  
  const auto& rules( picture_validation::picture_rules );
  // reset the global constraint structure. 
  auto& constraint(picture_validation::constraint);
  constraint = picture_validation::constraint_t();

  if( ! picture ) {
    return {picture, picture_validation::invalid_e};
  }
  const char *epicture = picture + strlen(picture);

  state_t state = picture_validation::none_e;
  for( auto p=picture; p < epicture; p++ ) {
    // Advance lookahead past any count, which might be name.
    // No picture begins with a '('. 
    if( picture < p && *p == '(' && p+1 < epicture  ) {
      p = seek_paren(p, epicture);
      if( *p != ')' ) {
        return {p, picture_validation::invalid_e};
      }
      if( p[-1] == '(' ) { // pic 9() is invalid
        return {p, picture_validation::invalid_e};
      }
      p++;
    }

    // Dot as last character is period separator because the lexer defines
    // end-of-picture as a space.
    if( end_of_picture(p, epicture) ) break;

    // Now p is fixed, but ch might be a currency symbol
    char ch = toupper(*p);
    
    // Is the current character ever allowed to follow the prior? 
    if( ! constraint.day_follows_night(ch) ) {
      if( symbol_currency(ch) ) {
        ch = '$';
      } else {
        if( verbose ) 
          fprintf(stderr,  "no %2zu: %c -> %c (%s)\n",
                  p - picture, constraint.prior(), ch,
                  picture_validation::state_str(constraint.automaton()) );
        return {p, picture_validation::invalid_e};
      }
    }

    // Consult the rules.
    picture_validation::transition_t query { state, ch, state };
    
    auto prule = std::lower_bound(rules.begin(), rules.end(), query);

    if( prule == rules.end() || *prule != query ) {
      state_t state = p == epicture?
        constraint.state() : picture_validation::invalid_e;
      return {p, state};
    }

    if( verbose ) 
      fprintf(stderr,  "ok %2zu: %c (%s)\n",
              p - picture, ch,
              picture_validation::state_str(prule->next_state) );

    // Advance to the next state.
    constraint = ch;
    state = prule->next_state;
  }

  return {epicture, constraint.state()};
}

#ifdef COBOL_PICTURE
/*
 * The test harness is not part of the compiler.  It can be used to create a
 * standalone executable for testing picture validation.
 */

static bool
process_line( const char picture[], int lineno = 1 ) {
  assert(picture);
  unsigned int mask = picture_validation::invalid_e;
  auto result = is_valid_picture(picture);
  if( 0 == (mask & result.second) ) {
    printf( "valid   : %s (%s)\n",
            picture, picture_validation::state_str(result.second) );
  } else {
    auto p = result.first;
    printf( "INVALID : %s at position %u '%c' (%s) line %d\n",
            picture, unsigned(p - picture), *p,
            picture_validation::state_str(result.second), lineno );
  }
  return result.second & picture_validation::invalid_e;
}

static bool
process_file(const char* filename) {
  auto input = stdin;
  if( filename ) {
    input = fopen(filename, "r");
    if( !input ) {
      fprintf(stderr, "could not open '%s': %m\n", filename);
      return false;
    }
  }
  
  char *picture = nullptr;
  size_t plen = 0;
  ssize_t len;
  int lineno = 0;
  
  while( (len = getline(&picture, &plen, input)) != -1 ) {
    auto p = std::find(picture, picture + len, '\n');
    if( p < picture + len ) *p = '\0';
    process_line(picture, ++lineno);
  }
  return len != -1;
}

int
main(int argc, char* argv[]) {
  int opt;
  bool fOK; 
  const char *picture = nullptr;

  while ((opt = getopt(argc, argv, "p:v")) != -1) {
    switch (opt) {
    case 'p':
      picture = optarg;
      break;
    case 'v':
      verbose = true;
      break;
    }
  }

  if (picture) {
    fOK = process_line(picture) ? 0 : 1;
  } else {
    if (optind < argc) {
      for (int i = optind; i < argc; ++i) {
        fOK = process_file(argv[i]);
      }
    } else {
      fOK = process_file(nullptr);
    }
  }
  
  return fOK ? 0 : 1;
}

#endif // COBOL_PICTURE
