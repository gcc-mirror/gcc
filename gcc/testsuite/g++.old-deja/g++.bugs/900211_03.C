// { dg-do assemble  }
// g++ 1.36.1 bug 900211_03

// The following erroneous code causes g++ to segfault.

// Cfront 2.0 passes this test.

// keywords: segfault, operator new, arrays, undeclared, array bound

void function ()
{
  char* new_base = new char[x];		// { dg-error "" } 
}

int main () { return 0; }
