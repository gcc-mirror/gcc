/* Require ilp32 because -mcpu=604 won't do 64 bits.  */
/* { dg-do compile { target { ilp32 } } } */
/* { dg-skip-if "No strict-align for AIX" { powerpc-ibm-aix* } } */
/* { dg-options "-mcpu=604 -O -mstrict-align" } */

union a {
  float _Complex b;
  long long c;
};

void g(union a);

void e() {
  union a f = {1.0f};
  g(f);
}
