/* PR c/89425 - -Wabsolute-value warns in dead subexpressions
   { dg-do compile }
   { dg-options "-Wabsolute-value -ftrack-macro-expansion=0" } */

struct Vals
{
  signed char sc;
  signed short ss;
  signed int si;
  signed long sl;
  signed long long sll;

  unsigned char uc;
  unsigned short us;
  unsigned int ui;
  unsigned long ul;
  unsigned long long ull;

  float f;
  double d;
  long double ld;
};

#define abs(x)     __builtin_abs (x)
#define labs(x)    __builtin_labs (x)
#define llabs(x)   __builtin_llabs (x)

#define fabsf(x)   __builtin_fabsf (x)
#define fabs(x)    __builtin_fabs (x)


void tst_warn (struct Vals *p)
{
  /* Verify that "-Wabsolute-value is issued for subexpressions
     that are evaluated.  */

  p->uc =  0 ? abs (p->sc) : abs (p->uc);         /* { dg-warning "\\\[-Wabsolute-value]" } */
  p->us =  0 ? abs (p->ss) : abs (p->us);         /* { dg-warning "\\\[-Wabsolute-value]" } */
  p->ui =  0 ? abs (p->si) : abs (p->ui);         /* { dg-warning "\\\[-Wabsolute-value]" } */
  p->ul =  0 ? labs (p->sl) : labs (p->ul);       /* { dg-warning "\\\[-Wabsolute-value]" } */
  p->ull = 0 ? llabs (p->sll) : llabs (p->ull);   /* { dg-warning "\\\[-Wabsolute-value]" } */

  p->d   = 0 ? fabs (p->d) : fabsf (p->d);        /* { dg-warning "\\\[-Wabsolute-value]" } */
}

void tst_no_warn (struct Vals *p)
{
  /* Verify that "-Wabsolute-value is not issued for subexpressions
     that are not evaluated.  */

  p->uc =  0 ? abs (p->uc) : abs (p->sc);
  p->us =  0 ? abs (p->us) : abs (p->ss);
  p->ui =  0 ? abs (p->ui) : abs (p->si);
  p->ul =  0 ? labs (p->ul) : labs (p->sl);
  p->ull = 0 ? llabs (p->ull) : llabs (p->sll);
  p->d   = 0 ? fabsf (p->d) : fabs (p->d);
}
