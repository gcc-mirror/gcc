// { dg-do assemble  }
// g++ 1.36.1 bug 900210_03

// g++ allows void* type values to be assigned to variables of other
// pointer types.  According to the C++ Reference Manual, this is illegal.

// Cfront 2.0 passes this test.

// keywords: void pointers, pointer type conversions, implicit type conversions

void* vp;
char* cp;
int* ip;
enum E {enum_value_1} * ep;
struct S { int member; } * sp;
void (*fp) (void);

void global_function ()
{
  cp = vp;	/* { dg-error "" }  */
  ip = vp;	/* { dg-error "" }  */
  ep = vp;	/* { dg-error "" }  */
  sp = vp;	/* { dg-error "" }  */
  fp = vp;	/* { dg-error "" }  */
}

int main () { return 0; }
