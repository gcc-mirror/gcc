/* Test that creal and cimag built-in functions do not return lvalues.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

extern float crealf (float _Complex);
extern double creal (double _Complex);
extern long double creall (long double _Complex);

extern float cimagf (float _Complex);
extern double cimag (double _Complex);
extern long double cimagl (long double _Complex);

float _Complex fc;
double _Complex dc;
long double _Complex ldc;

void
foo (void)
{
  crealf (fc) = 0; /* { dg-error "lvalue" "crealf not lvalue" } */
  cimagf (fc) = 0; /* { dg-error "lvalue" "cimagf not lvalue" } */
  creal (dc) = 0; /* { dg-error "lvalue" "creal not lvalue" } */
  cimag (dc) = 0; /* { dg-error "lvalue" "cimag not lvalue" } */
  creall (ldc) = 0; /* { dg-error "lvalue" "creall not lvalue" } */
  cimagl (ldc) = 0; /* { dg-error "lvalue" "cimagl not lvalue" } */
}
