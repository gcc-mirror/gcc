/*
 * Copyright (c) 2021-2026 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstring>

#include <unistd.h>

#include <algorithm>
#include <array>
#include <map>
#include <string>
#include <vector>

#include "cobol-system.h"
#include <coretypes.h>
#include <tree.h>
#include <fold-const.h>
#undef yy_flex_debug

#include <langinfo.h>

#include <version.h>
#include <demangle.h>
#include <intl.h>
#include <backtrace.h>
#include <diagnostic.h>
#include <opts.h>

#include "util.h"
#include "cbldiag.h"
#include "cdfval.h"
#include "lexio.h"

#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "symbols.h"

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
    { none_e,          'N',  national_e },
    { none_e,          '9',  antedec_e },   // 9 before V
    { none_e,          'P',  antedec_e },
    { none_e,          'S',  antedec_e },
    { none_e,          'V',  postdec_e, },  // V before 9

    { none_e,          '1',  boolean_e },
    { boolean_e,       '1',  boolean_e },

    { alphabetic_e,    'A',  alphabetic_e },
    { alphabetic_e,    'B',  alpha_ed_e },
    { alphabetic_e,    '0',  alpha_ed_e },
    { alphabetic_e,    '/',  alpha_ed_e },
    { alphabetic_e,    'X',  alphanumeric_e },
    { alphabetic_e,    '9',  alphanumeric_e },
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
    { antedec_e,       '+',  numeric_ed_e },
    { antedec_e,       '-',  numeric_ed_e },
    { antedec_e,       'C',  c_need_r_e },
    { antedec_e,       'D',  d_need_b_e },

    { antedec_e,       'Z',  numeric_ed_e },
    { antedec_e,       '*',  numeric_ed_e },
    { antedec_e,       'B',  numeric_ed_e },
    { antedec_e,       '0',  numeric_ed_e },
    { antedec_e,       '/',  numeric_ed_e },
    { antedec_e,       '$',  numeric_ed_e },
    { antedec_e,       dot,  numeric_ed_e },
    { antedec_e,       com,  numeric_ed_e },

    { postdec_e,       '9',  numeric_e },
    { postdec_e,       'P',  numeric_e },

    { numeric_e,       '9',  numeric_e },
    { numeric_e,       'P',  numeric_e },
    { numeric_e,       'V',  numeric_e },

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

  typedef std::array<state_t, 256> automaton_elem_t;
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

      p_alphanumeric_e = (p_A | p_X | p_nine),
      p_b_0_slash_e = (p_B | p_zero | p_slash),
      p_alpha_ed_e = (p_alphanumeric_e | p_b_0_slash_e),
      p_national_ed_e = (p_N | p_b_0_slash_e),
      p_numeric_e = (p_nine | p_P | p_S | p_V),
      // B P V Z 9 0 / , . + - CR DB * cs (CR/DB not in mask because at end)
      p_numeric_ed2_e = (p_b_0_slash_e | p_comma | p_dot | p_plus | p_minus),
      p_numeric_ed_e = ( p_P | p_V | p_Z | p_nine | p_b_0_slash_e |
                       p_comma | p_dot | p_plus | p_minus | p_star | p_cs),
    };
    uint64_t symbol_mask;

    bool both( picsym_t a, picsym_t b ) const {
      return (symbol_mask & (a | b)) == (a | b);
    }
    template <typename T>
    bool only(T bits) const {
      return symbol_mask == (symbol_mask & bits);
    }

    /*
     * 13.18.40.3 Syntax rules FORMAT 1
     */
    bool is_allowed( char ch ) const {
      switch( ch ) {
      case 'P':                       // no P in numeric-edited
      case 'V':                       // no V in numeric-edited
      case dot:                       // Neither P nor V with dot.
        if( both(p_P, p_dot) ) {      // rule 17
          error_msg(loc,  "%qc and %qc are mutually exclusive", 'P', dot);
          return false;
        }
        if( both(p_V, p_dot) ) {      // rule 20
          error_msg(loc,  "%qc and %qc are mutually exclusive", 'V', dot);
          return false;
        }
        break;
      case 'Z': case '*':             // Only blanks or stars
        if( both(p_Z, p_star) ) {     // rule 21
          error_msg(loc,  "%qc and %qc are mutually exclusive", 'Z', '*');
          return false;
        }
        break;
      }
      return true;
    }

    bool is_boolean() const    { return symbol_mask == p_one; }
    bool is_alphabetic() const { return symbol_mask == p_A; }
    bool is_national() const   { return symbol_mask == p_N; }

    bool is_alphanumeric() const {
      auto bits = symbol_mask & p_alphanumeric_e;
      return 0 < bits && symbol_mask == bits;
    }
    bool is_alpha_ed() const {
      return only(p_alpha_ed_e)
        &&   0 != (symbol_mask & p_alphanumeric_e)
        &&   0 != (symbol_mask & p_b_0_slash_e);
    }
    bool is_national_ed() const {
      return only(p_national_ed_e)
        &&   0 != (symbol_mask & p_N)
        &&   0 != (symbol_mask & p_b_0_slash_e);
    }
    bool is_numeric() const {
      return only(p_numeric_e)
        &&   0 != (symbol_mask &  p_nine)
        &&   0 != (symbol_mask &  p_numeric_e);
    }
    bool is_numeric9_ed() const {
      return only(p_numeric_ed_e)
        &&   0 != (symbol_mask & p_nine)
        &&   0 != (symbol_mask & p_numeric_ed2_e);
    }
    bool is_numeric_ed() const {
      return only(p_numeric_ed_e)
        && ( 0 != (symbol_mask & (p_Z | p_star))
             || two_signs()
             || is_numeric9_ed() );
    }

    bool two_signs() const { return 1 < nplus || 1 < nminus; }

    state_t pic_state; // to lookup sequence validity

    uint64_t picsym_set( char ch ) {
      switch(ch) {
      case '$': return symbol_mask |= p_cs;
      case '*': return symbol_mask |= p_star;
      case '+': return symbol_mask |= p_plus;
      case ',': return symbol_mask |= p_comma;
      case '-': return symbol_mask |= p_minus;
      case '.': return symbol_mask |= p_dot;
      case '/': return symbol_mask |= p_slash;
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
      case '$': return 0 < (symbol_mask & p_cs);
      case '*': return 0 < (symbol_mask & p_star);
      case '+': return 0 < (symbol_mask & p_plus);
      case ',': return 0 < (symbol_mask & p_comma);
      case '-': return 0 < (symbol_mask & p_minus);
      case '.': return 0 < (symbol_mask & p_dot);
      case '/': return 0 < (symbol_mask & p_slash);
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
      return false;
    }

    bool picsym_settable( char ch ) const {
      switch(ch) {
      case 'V':
        return ! picsym_seen(ch);
      case 'P':
        return ch == prior_ch || ! picsym_seen(ch);
      }
      return true;
    }

    cbl_loc_t loc;
    char prior_ch;
    const char decimal_point;
    int np, nplus, nminus, ndollar;
    uint64_t field_attr;
    cbl_field_data_t field_data;
    struct contiguous_t {
      char ch;
      int n;
      explicit contiguous_t( char ch ) : ch(ch),  n(1) {}
      contiguous_t& operator++() { n++; return *this; }
    };
    std::vector<contiguous_t> contiguous;

    // 13.18.40.3 Syntax rules #27 No more than 1 of certain character
    // sequences.  This function misinterprets the rule.  The rule is not that
    // e.g. $$,$$ is invalid.  the rule is that the list items are mutually
    // exclusive, $$Z is invalid.  Keeping the structure for now in case needed.
#if 0
    bool contiguous_ok(char ch) const {
      int n = 0;
      switch(ch) {
      case '+': case '-': case '$':
        n = 2;
        break;
      case 'Z': case '*':
        n = 1;
        break;
      }
      if( n ) {
        n = std::count_if( contiguous.begin(), contiguous.end(),
                           [ch, n]( const auto& elem ) {
                             return ch == elem.ch && n <= elem.n;
                           } );
      }
      return n < 2;
    }
#endif
    bool add_contiguous(char ch) {
      if( contiguous.empty() ) {
        contiguous.push_back( contiguous_t(ch) );
        return true;
      }
      auto& last( contiguous.back() );
      if( ch == last.ch ) {
        ++last;
      } else {
        contiguous.push_back( contiguous_t(ch) );
      }
      return true; // contiguous_ok(ch);
    }

   public:
    const char *bad_repeat;

    constraint_t(const cbl_loc_t& loc, const cbl_name_t picture)
      : symbol_mask(0)
      , pic_state(none_e)
      , loc(loc)
      , prior_ch('\0')
      , decimal_point(symbol_decimal_point())
      , np(0), nplus(0), nminus(0), ndollar(0)
      , field_attr(0)
      , bad_repeat(nullptr)
    {
      // Trim trailing punctuation from picture for later revalidation, because
      // we do.
      auto pic = xstrdup(picture);
      auto pend = pic + strlen(pic);
      if( pic < --pend ) {
        switch( *pend ) {
        case ';': case ',': case '.':
          *pend = '\0';
          break;
        }
      }

      field_data.picture = pic;
    }
    bool append( char ch, size_t len ) {
      if( prior_ch == '\0' ) {
        switch(ch) {
        case 'S': case '+': case '-':
          field_attr |= signable_e;
        }
      }
      if( prior_ch == '.' && ch == '.' ) return false;
      if( ! add_contiguous(ch) )         return false;

      if( ! picsym_settable(ch) ) { return false; }
      picsym_set(ch);
      // emits message about invalid combinations
      if( ! is_allowed(ch) ) { return false; }

      prior_ch = ch;
      add_capacity(len? len : 1);
      observe_the_dot(ch);
      loc++;
      return true;
    }
    inline void move_caret( int n ) { loc += n; }
    bool has_s_star() const { return both(p_S, p_star); }

    void extra_dot( size_t pos ) {
      if( field_data.picture[pos] != '\0') {
        assert(field_data.picture[pos+1] == '\0');
        assert(field_data.picture[pos] == '.');
        const_cast<char*>(field_data.picture)[pos] = '\0';
      }
    }

    bool repeatable() const {
      switch(prior_ch) {
      case '1':
      case 'P':
      case 'A': case 'X': case '9':
      case 'N':
      case 'U':
      case 'Z': case '*':
      case 'B': case '0': // B and 0 but not slash
      case '$': case '+': case '-':
        return true;
      }
      return false;
    }

    char prior() const { return prior_ch; }
    cbl_field_data_t data() const { return field_data; }
    uint64_t attr() const { return field_attr; }
    const cbl_loc_t& location() const { return loc; }

    state_t picture_state(state_t state) { return pic_state = state; }
    state_t picture_state() const { return pic_state; }

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

    void maybe_signable( char ch ) {
      switch(TOUPPER(ch)) {
      case '+': case '-':
        field_attr |= signable_e;
      }
      dbgmsg("%s: '%c', %ssignable", __func__,
             ch, (field_attr & signable_e)? "" : "not ");
    }

    void maybe_signable( state_t state ) {
      switch(state) {
      case c_need_r_e:
      case d_need_b_e:
        maybe_signable('+');
      default:
        break;
      }
    }

    /*
     * The followers table is sensitive to whether we've seen decimal point, and
     * whether we're in an exponent.  That reflects the duplicate column
     * headings in ISO Table 10.
     */
    bool day_follows_night( char ch ) const {
      const static std::string domain("*+,-./019ABENPSVXZCRDB");
      return std::string::npos != domain.find(ch);

      auto p = followers.find( follow_key_t {pic_state, prior_ch} );
      if( p != followers.end() ) {
        const auto& candidates(p->second);
        auto pnext = std::find( candidates.begin(), candidates.end(), ch );
        return pnext != candidates.end();
      }
      return false;
    }

    inline bool first_dollar( char ch ) { return ch == '$' && ndollar == 0; }

    /*
     * 1) Every character except V adds to the capacity, with the caveat that
     *    the first CURRENCY PICTURE character adds the length of the CURRENCY
     *    SIGN string.
     * 2) data.digits has to be the count of 9 + count of Z + count of asterisk
     *    + (count of $ minus 1) + (count of - minus one) + (count of + minus 1)
     * 3) data.rdigits is the subset of 2 that is to the right of either V or
     *    . (edited) 
     */
    void add_capacity(int n) {
      switch( prior_ch ) {
      case 'V':
        n = 0; // only in debug message
        break;
      case 'P':
        np += n;
        field_attr |= scaled_e;
        // P in front has positive rdigit less one; P aft is negative rdigit.
        {
          int ndigit = only(p_P | p_S)? (1 < np? n : n - 1) : -n;
          field_data.rdigits += ndigit;
        }
        n = 0; // only in debug message
        break;
      default:
        field_data.add_capacity(n);
        if( digital() ) {
          field_data.digits += n;
          if( pic_state == postdec_e ) {
            field_data.rdigits += n;
          }
        }
      }
      dbgmsg("%s: '%c', added %d: {%u, %u,%d}", __func__, prior_ch, n,
             field_data.capacity(), field_data.digits, field_data.rdigits);
    }

  protected:
    void observe_the_dot( char ch ) {
      if( pic_state == none_e ) pic_state = antedec_e;
      if( ch == decimal_point || ch == 'V' ) pic_state = postdec_e;
    }
    bool digital() {
      switch(prior_ch) {
      case '9': case 'Z': case '*':
        return true;
      case 'V':
        return false;
      case '+':
        return 0 < nplus++;
      case '-':
        return 0 < nminus++;
      case '$':
        return 0 < ndollar++;
      }
      return false;
    }
  };
} // end namespace

using picture_validation::transition_t;
using picture_validation::state_t;

/*
 * If a character within parentheses cannot be part of a COBOL word, then the
 * sequence is not an integer or data-item name.
 */
static const char *
seek_paren( const char *p, const char *epicture ) {
  for( p++; p < epicture; p++ ) {
    if( ! ISALNUM(*p) ) {
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

static bool verbose = true;

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

#include "semantic_token.h"
class picture_t : public lex_picture_t {
  state_t picture_state;
 public:
  picture_t()
    : picture_state(picture_validation::invalid_e)
  {
    attr = none_e;
    pos = 0;
    blank_when_zero_ok = true;
    type = FldInvalid;
    encoding = current_encoding('A');
    data = new cbl_field_data_t;
  }

  void state( state_t state ) {
    picture_state = state;
    type = field_type();
    encoding = field_encoding();
    if( picture_state == picture_validation::alphabetic_e ) {
      attr |= uint64_t(all_alpha_e);
    }
  }

  picture_t&  update( const picture_validation::constraint_t& constraint )
  {
    blank_when_zero_ok = ! constraint.has_s_star();
    attr = constraint.attr();
    *data = constraint.data();
    return *this;
  }

 protected:
  cbl_encoding_t field_encoding() const {
    switch(picture_state) {
    case picture_validation::unicode_e:
      cbl_unimplemented("Unicode pictures not implemented");
      return current_encoding('A');
    case picture_validation::national_e:
      return current_encoding('N');
    default: // alphanumeric by construction
      return current_encoding('A');
      break;
    }
  }
  cbl_field_type_t field_type() const  {
    switch(picture_state) {
    case picture_validation::alphabetic_e:
    case picture_validation::unicode_e:
    case picture_validation::alphanumeric_e:
    case picture_validation::national_e:
      return FldAlphanumeric;
    case picture_validation::alpha_ed_e:
    case picture_validation::national_ed_e:
      return FldAlphaEdited;
    case picture_validation::antedec_e:
    case picture_validation::numeric_e:
      return FldNumericDisplay;
    case picture_validation::numeric_ed_e:
      return FldNumericEdited;

    case picture_validation::invalid_e:
    case picture_validation::none_e:
    case picture_validation::boolean_e:
    case picture_validation::c_need_r_e:
    case picture_validation::d_need_b_e:
    case picture_validation::postdec_e:
    case picture_validation::exponent_e:
    case picture_validation::accept_e:
      break;
    }
    return FldInvalid;
  }
};

lex_picture_t
is_valid_picture(const cbl_loc_t& loc, const char picture[]) {
  picture_validation::constraint_t constraint(loc, picture);

  const picture_validation::automaton_matrix_t&
    automaton = picture_validation::prepare_automaton();

  if( ! picture ) {
    return lex_picture_t();
  }

  state_t state = picture_validation::none_e;
  const char *p = picture, *epicture = picture + strlen(picture);
  picture_t output;
  /*
   * If a currency has a symbol, width is set to its length until appended,
   * then becomes 1.
   */
  size_t width = 0;

  for( ; p < epicture; p++, output.pos++ ) {
    // Advance lookahead past any count, which might be a name.
    // No picture begins with a '('.
    if( *p == '(' && p+1 < epicture  ) {
      if( constraint.repeatable() ) {
        const char *paren = p;
        p = seek_paren(p, epicture);
        if( *p != ')' ) {
          output.pos += p - paren;
          return output;
        }
        if( p[-1] == '(' ) { // pic 9() is invalid
          output.pos += p - paren;
          return output;
        }
        constraint.move_caret(p - paren);
        p++;
        auto result = repeat_count(constraint.location(), --paren);
        int count = result.first;
        if( count == 0 ) {
          dbgmsg("%s: odd zero repeat count for '%s'", __func__, paren);
          constraint.bad_repeat = paren; // pointless, not used
          return output;
        }
        constraint.add_capacity(--count);
        output.pos = p - picture;
      }
    }

    // We may reach end of picture prematurely if it ends with ')'.
    if( end_of_picture(p, epicture) ) {
      epicture = p;
      break;
    }

    char ch = TOUPPER(*p);

    // Is the current character ever allowed to follow the prior?
    if( ! constraint.day_follows_night(ch) || constraint.first_dollar(ch)) {
      const char *currency = symbol_currency(ch);
      if( currency ) {
        ch = '$';
        width = width == 0? strlen(currency) : 1;
      } else {
        dbgmsg("%s:%d: no %2lu: %c -> %c (%s)\n", __func__, __LINE__,
               (unsigned long)(p - picture), constraint.prior(), ch,
               picture_validation::state_str(constraint.picture_state()));
        return output;
      }
      if( verbose ) {
        dbgmsg("ok %2lu: %c (next: %s)\n", (unsigned long)(p - picture), ch,
               picture_validation::state_str(state));
      }
    }

    // Update constraint record and advance to the next state.
    const auto old_state = picture_validation::state_str(state);
    if( constraint.append(ch, width) ) {
      if( 1 < width ) width = 1;
      state = automaton[state][ch];
      constraint.maybe_signable(state);
    } else {
      state = picture_validation::invalid_e;
    }

    if( state == picture_validation::invalid_e ) {
      dbgmsg("%s:%d: automaton has no transition for %s '%c'",
             __func__, __LINE__, old_state, ch);
      return output;
    }
  }

  assert( p == epicture );

  if( picture < p ) {
    char ch = p == epicture? p[-1] : p[0];
    constraint.maybe_signable(ch);
  }

  output.state(state);
  output.update(constraint);

  dbgmsg("%s:%d: decided on %s", __func__, __LINE__,
         cbl_field_type_str(output.type));

  return output;
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
