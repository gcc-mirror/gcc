// { dg-do assemble  }
// g++ 1.36.1 bug 900210_08

// g++ allows pointer-to-const values to be implicitly converted to
// void* values.  This causes a silent loss of the const qualifier.

// Cfront 2.0 passes this test.

// keywords: pointer types, implicit type conversions

const char *ccp;
void *vp;

void function ()
{
  vp = ccp;		/* { dg-error "" } */
}

int main () { return 0; }
