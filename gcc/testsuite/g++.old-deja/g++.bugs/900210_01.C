// { dg-do assemble  }
// g++ 1.36.1 bug 900210_01

// g++ allows pointer type values to be assigned to variables of integal
// types.  According to the C++ Reference Manual, this is illegal.

// Cfront 2.0 passes this test.

// keywords: pointer types, integral types, implicit type conversions

int i;
long l;
short s;
char c;
float f;
double d;
long double ld;
enum E {enum_value_0} e;

signed int si;
signed long sl;
signed short ss;
signed char sc;

unsigned int ui;
unsigned long ul;
unsigned short us;
unsigned char uc;

void* vp;
char* cp;
int* ip;
enum E2 {enum_value_1} * ep;
struct S { int member; } * sp;
void (*fp) (void);

void global_function ()
{
  i = vp;	/* { dg-error "" }  */
  i = cp;	/* { dg-error "" }  */
  i = ip;	/* { dg-error "" }  */
  i = ep;	/* { dg-error "" }  */
  i = sp;	/* { dg-error "" }  */
  i = fp;	/* { dg-error "" }  */
  l = vp;	/* { dg-error "" }  */
  l = cp;	/* { dg-error "" }  */
  l = ip;	/* { dg-error "" }  */
  l = ep;	/* { dg-error "" }  */
  l = sp;	/* { dg-error "" }  */
  l = fp;	/* { dg-error "" }  */
  s = vp;	/* { dg-error "" }  */
  s = cp;	/* { dg-error "" }  */
  s = ip;	/* { dg-error "" }  */
  s = ep;	/* { dg-error "" }  */
  s = sp;	/* { dg-error "" }  */
  s = fp;	/* { dg-error "" }  */
  c = vp;	/* { dg-error "" }  */
  c = cp;	/* { dg-error "" }  */
  c = ip;	/* { dg-error "" }  */
  c = ep;	/* { dg-error "" }  */
  c = sp;	/* { dg-error "" }  */
  c = fp;	/* { dg-error "" }  */
  si = vp;	/* { dg-error "" }  */
  si = cp;	/* { dg-error "" }  */
  si = ip;	/* { dg-error "" }  */
  si = ep;	/* { dg-error "" }  */
  si = sp;	/* { dg-error "" }  */
  si = fp;	/* { dg-error "" }  */
  sl = vp;	/* { dg-error "" }  */
  sl = cp;	/* { dg-error "" }  */
  sl = ip;	/* { dg-error "" }  */
  sl = ep;	/* { dg-error "" }  */
  sl = sp;	/* { dg-error "" }  */
  sl = fp;	/* { dg-error "" }  */
  ss = vp;	/* { dg-error "" }  */
  ss = cp;	/* { dg-error "" }  */
  ss = ip;	/* { dg-error "" }  */
  ss = ep;	/* { dg-error "" }  */
  ss = sp;	/* { dg-error "" }  */
  ss = fp;	/* { dg-error "" }  */
  sc = vp;	/* { dg-error "" }  */
  sc = cp;	/* { dg-error "" }  */
  sc = ip;	/* { dg-error "" }  */
  sc = ep;	/* { dg-error "" }  */
  sc = sp;	/* { dg-error "" }  */
  sc = fp;	/* { dg-error "" }  */
  ui = vp;	/* { dg-error "" }  */
  ui = cp;	/* { dg-error "" }  */
  ui = ip;	/* { dg-error "" }  */
  ui = ep;	/* { dg-error "" }  */
  ui = sp;	/* { dg-error "" }  */
  ui = fp;	/* { dg-error "" }  */
  ul = vp;	/* { dg-error "" }  */
  ul = cp;	/* { dg-error "" }  */
  ul = ip;	/* { dg-error "" }  */
  ul = ep;	/* { dg-error "" }  */
  ul = sp;	/* { dg-error "" }  */
  ul = fp;	/* { dg-error "" }  */
  us = vp;	/* { dg-error "" }  */
  us = cp;	/* { dg-error "" }  */
  us = ip;	/* { dg-error "" }  */
  us = ep;	/* { dg-error "" }  */
  us = sp;	/* { dg-error "" }  */
  us = fp;	/* { dg-error "" }  */
  uc = vp;	/* { dg-error "" }  */
  uc = cp;	/* { dg-error "" }  */
  uc = ip;	/* { dg-error "" }  */
  uc = ep;	/* { dg-error "" }  */
  uc = sp;	/* { dg-error "" }  */
  uc = fp;	/* { dg-error "" }  */
}

int main () { return 0; }
