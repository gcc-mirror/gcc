// { dg-do assemble  }
// g++ 1.37.1 bug 900324_02

// The following erroreous code causes g++ to segfault.

// Cfront 2.0 passes this test.

// keywords: segfault, function pointer, conditional operator ?:

void function_0 (int i) { }

void (*fp)(void);

void function_1 ()
{
  fp = 1 ? function_0 : fp;		// { dg-error "" } 
}

int main () { return 0; }
