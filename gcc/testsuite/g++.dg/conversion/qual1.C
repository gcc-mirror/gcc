// PR c++/88128 - Implement DR 330: Qualification conversions and pointers to
// arrays of pointers.

int *a[4];
const int *const(*ap1)[4] = &a;
/* if at some level k the P2 is more cv-qualified than P1, then there
   must be a const at every single level (other than level zero) of P2
   up until k.  */
const int *(*ap2)[4] = &a; // { dg-error "cannot convert" }
int *const(*ap3)[4] = &a;
int *(*ap4)[4] = &a;
int *(*const ap5)[4] = &a;
const int *const(*const ap6)[4] = &a;
int *const(*const ap7)[4] = &a;
int *(*const ap8)[4] = &a;

const int *b[4];
const int *const(*bp1)[4] = &b;
const int *(*bp2)[4] = &b;
int *const(*bp3)[4] = &b; // { dg-error "cannot convert" }
int *(*bp4)[4] = &b; // { dg-error "cannot convert" }
int *(*const bp5)[4] = &b; // { dg-error "cannot convert" }
const int *const(*const bp6)[4] = &b;
int *const(*const bp7)[4] = &b; // { dg-error "cannot convert" }
int *(*const bp8)[4] = &b; // { dg-error "cannot convert" }

int *c[2][3];
int const *const (*cp1)[3] = c;
int const *(*cp2)[3] = c; // { dg-error "cannot convert" }
int const *const (*const cp3)[3] = c;
int *const (*cp4)[3] = c;
int *(*cp5)[3] = c;

double *const (*d)[3];
double const *const (*e)[3] = d;
int *(*f)[3];
const int *const (*g)[3] = f;

// From PR88128.
int* (*xx)[];
const int* const(*yy)[] = xx;

// From DR 330.
int main()
{
   double *array2D[2][3];

   double       *       (*array2DPtr1)[3] = array2D;
   double       * const (*array2DPtr2)[3] = array2DPtr1;
   double const * const (*array2DPtr3)[3] = array2DPtr2;
}
