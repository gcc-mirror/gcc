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
  vp = i;	/* ERROR -  */
  vp = l;	/* ERROR -  */
  vp = s;	/* ERROR -  */
  vp = c;	/* ERROR -  */
  vp = si;	/* ERROR -  */
  vp = sl;	/* ERROR -  */
  vp = ss;	/* ERROR -  */
  vp = sc;	/* ERROR -  */
  vp = ui;	/* ERROR -  */
  vp = ul;	/* ERROR -  */
  vp = us;	/* ERROR -  */
  vp = uc;	/* ERROR -  */
  cp = i;	/* ERROR -  */
  cp = l;	/* ERROR -  */
  cp = s;	/* ERROR -  */
  cp = c;	/* ERROR -  */
  cp = si;	/* ERROR -  */
  cp = sl;	/* ERROR -  */
  cp = ss;	/* ERROR -  */
  cp = sc;	/* ERROR -  */
  cp = ui;	/* ERROR -  */
  cp = ul;	/* ERROR -  */
  cp = us;	/* ERROR -  */
  cp = uc;	/* ERROR -  */
  ip = i;	/* ERROR -  */
  ip = l;	/* ERROR -  */
  ip = s;	/* ERROR -  */
  ip = c;	/* ERROR -  */
  ip = si;	/* ERROR -  */
  ip = sl;	/* ERROR -  */
  ip = ss;	/* ERROR -  */
  ip = sc;	/* ERROR -  */
  ip = ui;	/* ERROR -  */
  ip = ul;	/* ERROR -  */
  ip = us;	/* ERROR -  */
  ip = uc;	/* ERROR -  */
  ep = i;	/* ERROR -  */
  ep = l;	/* ERROR -  */
  ep = s;	/* ERROR -  */
  ep = c;	/* ERROR -  */
  ep = si;	/* ERROR -  */
  ep = sl;	/* ERROR -  */
  ep = ss;	/* ERROR -  */
  ep = sc;	/* ERROR -  */
  ep = ui;	/* ERROR -  */
  ep = ul;	/* ERROR -  */
  ep = us;	/* ERROR -  */
  ep = uc;	/* ERROR -  */
  sp = i;	/* ERROR -  */
  sp = l;	/* ERROR -  */
  sp = s;	/* ERROR -  */
  sp = c;	/* ERROR -  */
  sp = si;	/* ERROR -  */
  sp = sl;	/* ERROR -  */
  sp = ss;	/* ERROR -  */
  sp = sc;	/* ERROR -  */
  sp = ui;	/* ERROR -  */
  sp = ul;	/* ERROR -  */
  sp = us;	/* ERROR -  */
  sp = uc;	/* ERROR -  */
  fp = i;	/* ERROR -  */
  fp = l;	/* ERROR -  */
  fp = s;	/* ERROR -  */
  fp = c;	/* ERROR -  */
  fp = si;	/* ERROR -  */
  fp = sl;	/* ERROR -  */
  fp = ss;	/* ERROR -  */
  fp = sc;	/* ERROR -  */
  fp = ui;	/* ERROR -  */
  fp = ul;	/* ERROR -  */
  fp = us;	/* ERROR -  */
  fp = uc;	/* ERROR -  */
}

int main () { return 0; }
