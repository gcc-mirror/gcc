// g++ 1.36.1 bug 900210_07

// g++ allows values of pointer-to-signed types to be assigned to variables
// of pointer-to-unsigned types, and vise versa.

// Cfront 2.0 passes this test.

// keyowrds: pointer types, implicit type conversions
// Special Options: -ansi -pedantic-errors
signed int *sip;
unsigned int *uip;

void function ()
{
  sip = uip;		// ERROR - 
  uip = sip;		// ERROR - 
}

int main () { return 0; }
