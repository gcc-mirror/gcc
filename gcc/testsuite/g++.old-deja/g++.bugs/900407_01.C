// g++ 1.37.1 bug 900407_01

// g++ fails to flag errors for uses of anachronistic features such as the
// invocation of a base class constructor in a ctor-initializer list without
// explicitly giving its name.

// Errors should probably be issued for such usage unless the -traditional
// option is used.

// Warnings are however issued.

// Cfront 2.0 flags such usage as an error when the +p (pure-language) option
// is used.

// Cfront 2.0 passes this test.

// keywords: anachronism, inheritance, initialization, mem-initializer

struct s0 {
  int member;

  s0 ();
};

s0::s0() { }

struct s1 : public s0 {
  int member;

  s1 ();
};

s1::s1() : () {		// ERROR - anachronism used
}

int main () { return 0; }
