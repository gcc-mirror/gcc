// g++ 1.37.1 bug 900407_02

// g++ flags errors for attempts to assign to the "this" parameter within
// class constructors (regardless of whether or not the -traditional)
// option is used).

// Such errors should probably not be issued when the -traditional option is
// used.

// Special g++ Options: -Wno-deprecated -fthis-is-variable
// Special CC Options:

// Cfront only flags errors for such usage whin the +p (pure language)
// option is used.

// cfront 2.0 passes this test.

// keywords: anachronism, this, assignment

struct s0 {

  int member;

  s0 ();
};

s0::s0 ()
{
  this = this;		// OK - anachronism allowed with -traditional
}

int main () { return 0; }
