// { dg-do run  }
// g++ 1.36.1 bug 900220_02

// g++ treats plain `char' and `unsigned char' as different types, however
// it fails to treat `signed char' as being a different type from plain
// `char' as called for by both the ANSI C standard and the C++ reference
// manual.

// keywords: plain char type, signed char type, unsigned char type, overloading

void overloaded (char) {
}

void overloaded (signed char) {		// { dg-bogus "" } 
}

void overloaded (unsigned char) {
}

void global_function ()
{
  char c = 0;
  signed char sc = 0;
  unsigned char uc = 0;

  overloaded (c);
  overloaded (sc);
  overloaded (uc);
}

int main () { return 0; }
