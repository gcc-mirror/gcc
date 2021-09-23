/* PR middle-end/101601 - [12 Regression] -Warray-bounds triggers error:
   arrays of functions are not meaningful
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef void Fvv (void);

extern Fvv* pf;       // { dg-message "'pf'" }

void f (void*);

void test_funptr (void)
{
  f (&pf);
  f (&pf + 1);
  f (&pf + 2);        // { dg-warning "subscript 2 is outside array bounds of 'void \\\(\\\*\\\[1]\\\)\\\(void\\\)'" }
}

typedef int Fii_ (int, ...);

extern Fii_* pfa[3];  // { dg-message "'pfa'" }

void test_funptr_array (void)
{
  f (pfa);
  f (pfa + 1);
  f (pfa + 2);
  f (pfa + 3);
  f (pfa + 4);        // { dg-warning "subscript 4 is outside array bounds of 'int \\\(\\\*\\\[3]\\\)\\\(int, ...\\\)'" }
}
