// { dg-do assemble  }
// g++ 1.36.1 bug 900210_06

// g++ allows values of pointer-to-const types to be assigned to variables
// of pointer-to-non-const types.

// Cfront 2.0 disallows such assignments.

// g++ also allows values of pointer-to-volatile types to be assigned to
// variables of pointer-to-non-volatile types.

// Cfront 2.0 *would* disallow this (if it only supported "volatile").

// keywords: pointer types, implicit type conversions

const char *ccp;
volatile char *vcp;
char *cp;

void function ()
{
  cp = ccp;		/* { dg-error "" } */
  cp = vcp;		/* { dg-error "" } */
}

int main () { return 0; }
