// { dg-do assemble  }
// { dg-options "-w" }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }
// GROUPS passed old-abort
//#include <GetOpt.h>
#include <stdio.h>

class GetOpt
{
private:
  static char *nextchar;
   enum OrderingEnum { REQUIRE_ORDER, PERMUTE, RETURN_IN_ORDER };
   OrderingEnum ordering;
  static int first_nonopt;
  static int last_nonopt;
  void exchange (char **argv);
public:
  char *optarg;
  int optind;
  int opterr;
  
  int    nargc;
  char **nargv;
  const char  *noptstring;
  
  GetOpt (int argc, char **argv, const char *optstring);
  int operator () (void);
};
//end <GetOpt.h>
#include <string>

class foo {public: foo () {}};
class bar {public: bar (const foo& dflt);};
class baz: public bar {public: baz (): bar (foo ()) {}};
