// { dg-do assemble  }
// { dg-options "" }
// g++ 1.36.1 bug 900210_02

// g++ allows integral type values to be assigned to variables of pointer
// types.  According to the C++ Reference Manual, this is illegal.

// Cfront 2.0 passes this test.

// keywords: integral types, pointer types, implicit type conversions
// Special Options: -ansi -pedantic-errors
int i;
long l;
short s;
char c;
float f;
double d;
long double ld;
enum {enum_value_0} e;

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
enum {enum_value_1} * ep;
struct { int member; } * sp;
void (*fp) (void);

void global_function ()
{
  vp = i;	/* { dg-error "" }  */
  vp = l;	/* { dg-error "" }  */
  vp = s;	/* { dg-error "" }  */
  vp = c;	/* { dg-error "" }  */
  vp = si;	/* { dg-error "" }  */
  vp = sl;	/* { dg-error "" }  */
  vp = ss;	/* { dg-error "" }  */
  vp = sc;	/* { dg-error "" }  */
  vp = ui;	/* { dg-error "" }  */
  vp = ul;	/* { dg-error "" }  */
  vp = us;	/* { dg-error "" }  */
  vp = uc;	/* { dg-error "" }  */
  cp = i;	/* { dg-error "" }  */
  cp = l;	/* { dg-error "" }  */
  cp = s;	/* { dg-error "" }  */
  cp = c;	/* { dg-error "" }  */
  cp = si;	/* { dg-error "" }  */
  cp = sl;	/* { dg-error "" }  */
  cp = ss;	/* { dg-error "" }  */
  cp = sc;	/* { dg-error "" }  */
  cp = ui;	/* { dg-error "" }  */
  cp = ul;	/* { dg-error "" }  */
  cp = us;	/* { dg-error "" }  */
  cp = uc;	/* { dg-error "" }  */
  ip = i;	/* { dg-error "" }  */
  ip = l;	/* { dg-error "" }  */
  ip = s;	/* { dg-error "" }  */
  ip = c;	/* { dg-error "" }  */
  ip = si;	/* { dg-error "" }  */
  ip = sl;	/* { dg-error "" }  */
  ip = ss;	/* { dg-error "" }  */
  ip = sc;	/* { dg-error "" }  */
  ip = ui;	/* { dg-error "" }  */
  ip = ul;	/* { dg-error "" }  */
  ip = us;	/* { dg-error "" }  */
  ip = uc;	/* { dg-error "" }  */
  ep = i;	/* { dg-error "" }  */
  ep = l;	/* { dg-error "" }  */
  ep = s;	/* { dg-error "" }  */
  ep = c;	/* { dg-error "" }  */
  ep = si;	/* { dg-error "" }  */
  ep = sl;	/* { dg-error "" }  */
  ep = ss;	/* { dg-error "" }  */
  ep = sc;	/* { dg-error "" }  */
  ep = ui;	/* { dg-error "" }  */
  ep = ul;	/* { dg-error "" }  */
  ep = us;	/* { dg-error "" }  */
  ep = uc;	/* { dg-error "" }  */
  sp = i;	/* { dg-error "" }  */
  sp = l;	/* { dg-error "" }  */
  sp = s;	/* { dg-error "" }  */
  sp = c;	/* { dg-error "" }  */
  sp = si;	/* { dg-error "" }  */
  sp = sl;	/* { dg-error "" }  */
  sp = ss;	/* { dg-error "" }  */
  sp = sc;	/* { dg-error "" }  */
  sp = ui;	/* { dg-error "" }  */
  sp = ul;	/* { dg-error "" }  */
  sp = us;	/* { dg-error "" }  */
  sp = uc;	/* { dg-error "" }  */
  fp = i;	/* { dg-error "" }  */
  fp = l;	/* { dg-error "" }  */
  fp = s;	/* { dg-error "" }  */
  fp = c;	/* { dg-error "" }  */
  fp = si;	/* { dg-error "" }  */
  fp = sl;	/* { dg-error "" }  */
  fp = ss;	/* { dg-error "" }  */
  fp = sc;	/* { dg-error "" }  */
  fp = ui;	/* { dg-error "" }  */
  fp = ul;	/* { dg-error "" }  */
  fp = us;	/* { dg-error "" }  */
  fp = uc;	/* { dg-error "" }  */
}

int main () { return 0; }
