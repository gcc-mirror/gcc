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
  i = vp;	/* ERROR -  */
  i = cp;	/* ERROR -  */
  i = ip;	/* ERROR -  */
  i = ep;	/* ERROR -  */
  i = sp;	/* ERROR -  */
  i = fp;	/* ERROR -  */
  l = vp;	/* ERROR -  */
  l = cp;	/* ERROR -  */
  l = ip;	/* ERROR -  */
  l = ep;	/* ERROR -  */
  l = sp;	/* ERROR -  */
  l = fp;	/* ERROR -  */
  s = vp;	/* ERROR -  */
  s = cp;	/* ERROR -  */
  s = ip;	/* ERROR -  */
  s = ep;	/* ERROR -  */
  s = sp;	/* ERROR -  */
  s = fp;	/* ERROR -  */
  c = vp;	/* ERROR -  */
  c = cp;	/* ERROR -  */
  c = ip;	/* ERROR -  */
  c = ep;	/* ERROR -  */
  c = sp;	/* ERROR -  */
  c = fp;	/* ERROR -  */
  si = vp;	/* ERROR -  */
  si = cp;	/* ERROR -  */
  si = ip;	/* ERROR -  */
  si = ep;	/* ERROR -  */
  si = sp;	/* ERROR -  */
  si = fp;	/* ERROR -  */
  sl = vp;	/* ERROR -  */
  sl = cp;	/* ERROR -  */
  sl = ip;	/* ERROR -  */
  sl = ep;	/* ERROR -  */
  sl = sp;	/* ERROR -  */
  sl = fp;	/* ERROR -  */
  ss = vp;	/* ERROR -  */
  ss = cp;	/* ERROR -  */
  ss = ip;	/* ERROR -  */
  ss = ep;	/* ERROR -  */
  ss = sp;	/* ERROR -  */
  ss = fp;	/* ERROR -  */
  sc = vp;	/* ERROR -  */
  sc = cp;	/* ERROR -  */
  sc = ip;	/* ERROR -  */
  sc = ep;	/* ERROR -  */
  sc = sp;	/* ERROR -  */
  sc = fp;	/* ERROR -  */
  ui = vp;	/* ERROR -  */
  ui = cp;	/* ERROR -  */
  ui = ip;	/* ERROR -  */
  ui = ep;	/* ERROR -  */
  ui = sp;	/* ERROR -  */
  ui = fp;	/* ERROR -  */
  ul = vp;	/* ERROR -  */
  ul = cp;	/* ERROR -  */
  ul = ip;	/* ERROR -  */
  ul = ep;	/* ERROR -  */
  ul = sp;	/* ERROR -  */
  ul = fp;	/* ERROR -  */
  us = vp;	/* ERROR -  */
  us = cp;	/* ERROR -  */
  us = ip;	/* ERROR -  */
  us = ep;	/* ERROR -  */
  us = sp;	/* ERROR -  */
  us = fp;	/* ERROR -  */
  uc = vp;	/* ERROR -  */
  uc = cp;	/* ERROR -  */
  uc = ip;	/* ERROR -  */
  uc = ep;	/* ERROR -  */
  uc = sp;	/* ERROR -  */
  uc = fp;	/* ERROR -  */
}

int main () { return 0; }
