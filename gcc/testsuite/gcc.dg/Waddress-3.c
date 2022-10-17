/* PR c/102103 - missing warning comparing array address to null
   { dg-do compile }
   { dg-options "-Wall" } */

typedef _Complex float Cflt;

extern Cflt cf, cfa[], cfa2[][2];

Cflt *pcf (void);

void T (int);

void test_complex (Cflt *p, int i)
{
  T (&__real__ cf == 0);              // { dg-warning "address of '__real__ cf'" }
  T (&__imag__ cf == 0);              // { dg-warning "address of '__imag__ cf'" }

  T (0 != &__real__ cf);              // { dg-warning "-Waddress" }
  T (0 != &__imag__ cf);              // { dg-warning "-Waddress" }

  T (&__real__ cfa[0] == 0);          // { dg-warning "-Waddress" }
  T (&__imag__ cfa[1] == 0);          // { dg-warning "-Waddress" }

  T (0 != &__real__ cfa2[i][i]);      // { dg-warning "-Waddress" }
  T (0 != &__imag__ cfa2[i][i]);      // { dg-warning "-Waddress" }

  T (0 == &__real__ *p);              // { dg-warning "-Waddress" }
  T (0 == &__imag__ *p);              // { dg-warning "-Waddress" }

  T (0 == &__real__ p[i]);            // { dg-warning "-Waddress" }
  T (0 == &__imag__ p[i]);            // { dg-warning "-Waddress" }

  T (&__real__ *pcf () == 0);         // { dg-warning "-Waddress" }
  T (0 != &__imag__ *pcf ());         // { dg-warning "-Waddress" }
}
