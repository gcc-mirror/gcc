/*
 * Contributed to the public domain by James K. Lowden
 * Tuesday October 17, 2023
 *
 * This stand-in for std::regex was written because the implementation provided
 * by the GCC libstdc++ in GCC 11 proved too slow, where "slow" means "appears
 * not to terminate".  Some invocations of std::regex_search took over 5
 * seconds (or minutes) and used over 1900 stack frames, and "never" returned.
 * Because the same patterns and input presented no difficulty to the C standad
 * library regex functions, I recast the C++ implementation in terms of
 * regex(3).
 *
 * Unlike std::regex, this dts version supports only Posix EREs, and requires
 * the input to be NUL-terminated.
 *
 * It is my hope and expectation to replace this implementation with the
 * standard one when it is improved.
 */

#include <stdexcept>
#include <vector>

#include <regex.h>

namespace dts {
  class csub_match : public regmatch_t {
    const char *input;
  public:
    const char *first, *second;
    bool matched;

    explicit csub_match( const char *input = NULL)
      : input(input)
      , first(NULL), second(NULL), matched(false)
    {
      static regmatch_t empty;
      empty.rm_so = empty.rm_eo = -1;
      regmatch_t& self(*this); // cppcheck-suppress constVariableReference
      self = empty;
    }
    csub_match( const char input[], const regmatch_t& m )
      : input(input)
    {
      regmatch_t& self(*this); // cppcheck-suppress constVariableReference
      self = m;
      matched = rm_so != -1;
      first =   rm_so == -1? NULL : input + rm_so;
      second =  rm_eo == -1? NULL : input + rm_eo;
    }

    int length() const { return rm_eo - rm_so; }
  };

  typedef std::vector<csub_match> cmatch;

  class regex : public regex_t {
    size_t nsubexpr;
    const char *pattern;
  public:
    enum cflag_t { extended = REG_EXTENDED, icase = REG_ICASE };

    regex( const char pattern[], int flags ) : pattern(pattern) {
      nsubexpr = 1 + std::count(pattern, pattern + strlen(pattern), '(');
      int erc = regcomp(this, pattern, flags);
      if( erc != 0 ) {
        char msg[80];
        regerror(erc, this, msg, sizeof msg);
#if __cpp_exceptions
        throw std::logic_error(msg);
#else
        cbl_errx("%s", msg);
#endif
      }
    }
    ~regex() { regfree(this); }

    size_t size() const { return nsubexpr; }
    bool ready() const { return pattern != NULL; }
  private:
    regex( const regex& ) = default;
  };

  inline bool regex_search( const char input[], const char *eoinput,
                     cmatch& cm, regex& re ) {
    if( eoinput != NULL && *eoinput != '\0' ) {
#if __cpp_exceptions
      static const char msg[] = "input not NUL-terminated";
      throw std::domain_error( msg );
#endif
    }
    auto ncm = re.size();
    cm.resize(ncm);
    std::vector <regmatch_t> cms(ncm);

    int erc = regexec( &re, input, ncm, cms.data(), 0 );
    if( erc != 0 ) return false;
#if  __cpp_exceptions
    // This is not correct at all, but current use depends on current behavior.
    // The following line is excluded from the GCC build, which is compiled
    // without __cpp_exceptions.  parse_copy_directive (for one) depends on
    // regex_search returning true even if the match is beyond eoinput.
    if( eoinput < cm[0].second ) return false;
    // Correct behavior would return match only between input and eoinput.
    // Because regex(3) uses a NUL terminator, it may match text between
    // eoinput and the NUL.
#endif
    std::transform( cms.begin(), cms.end(), cm.begin(),
                    [input]( const regmatch_t& m ) {
                      return csub_match( input, m );
                    } );
    return true;
  }
}


