// Build don't link: 
// Special g++ Options: -w
// GROUPS passed old-abort
#include <GetOpt.h>
#include <String.h>
class foo {public: foo () {}};
class bar {public: bar (foo& dflt);};
class baz: public bar {public: baz (): bar (foo ()) {}};
