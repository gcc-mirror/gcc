/* Test messages for -Wconversion, including that they are not
   pedwarns.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors -Wconversion" } */

void fsc(signed char);
void fsi(signed int);
void fsll(signed long long);
void fuc(unsigned char);
void fui(unsigned int);
void full(unsigned long long);
void ff(float);
void fld(long double);
void fcf(_Complex float);

struct s {
  void (*fsc)(signed char);
  void (*fsi)(signed int);
  void (*fsll)(signed long long);
  void (*fuc)(unsigned char);
  void (*fui)(unsigned int);
  void (*full)(unsigned long long);
  void (*ff)(float);
  void (*fld)(long double);
  void (*fcf)(_Complex float);
} x;

signed char sc;
signed int si;
signed long long sll;
unsigned char uc;
unsigned int ui;
unsigned long long ull;
float f;
long double ld;
_Complex float cf;

void
g (void)
{
  fsi(f); /* { dg-warning "warning: passing argument 1 of 'fsi' as integer rather than floating due to prototype" } */
  x.fsi(f); /* { dg-warning "warning: passing argument 1 of 'x.fsi' as integer rather than floating due to prototype" } */
  fsi(cf); /* { dg-warning "warning: passing argument 1 of 'fsi' as integer rather than complex due to prototype" } */
  x.fsi(cf); /* { dg-warning "warning: passing argument 1 of 'x.fsi' as integer rather than complex due to prototype" } */
  fcf(f); /* { dg-warning "warning: passing argument 1 of 'fcf' as complex rather than floating due to prototype" } */
  x.fcf(f); /* { dg-warning "warning: passing argument 1 of 'x.fcf' as complex rather than floating due to prototype" } */
  fcf(si); /* { dg-warning "warning: passing argument 1 of 'fcf' as complex rather than integer due to prototype" } */
  x.fcf(si); /* { dg-warning "warning: passing argument 1 of 'x.fcf' as complex rather than integer due to prototype" } */
  ff(sc); /* { dg-warning "warning: passing argument 1 of 'ff' as floating rather than integer due to prototype" } */
  x.ff(sc); /* { dg-warning "warning: passing argument 1 of 'x.ff' as floating rather than integer due to prototype" } */
  ff(cf); /* { dg-warning "warning: passing argument 1 of 'ff' as floating rather than complex due to prototype" } */
  x.ff(cf); /* { dg-warning "warning: passing argument 1 of 'x.ff' as floating rather than complex due to prototype" } */
  ff(1.0); /* { dg-warning "warning: passing argument 1 of 'ff' as 'float' rather than 'double' due to prototype" } */
  x.ff(1.0); /* { dg-warning "warning: passing argument 1 of 'x.ff' as 'float' rather than 'double' due to prototype" } */
  fsll(sc); /* { dg-warning "warning: passing argument 1 of 'fsll' with different width due to prototype" } */
  x.fsll(sc); /* { dg-warning "warning: passing argument 1 of 'x.fsll' with different width due to prototype" } */
  fsc(sll); /* { dg-warning "warning: passing argument 1 of 'fsc' with different width due to prototype" } */
  x.fsc(sll); /* { dg-warning "warning: passing argument 1 of 'x.fsc' with different width due to prototype" } */
  fsi(ui); /* { dg-warning "warning: passing argument 1 of 'fsi' as signed due to prototype" } */
  x.fsi(ui); /* { dg-warning "warning: passing argument 1 of 'x.fsi' as signed due to prototype" } */
  full(sll); /* { dg-warning "warning: passing argument 1 of 'full' as unsigned due to prototype" } */
  x.full(sll); /* { dg-warning "warning: passing argument 1 of 'x.full' as unsigned due to prototype" } */
}
