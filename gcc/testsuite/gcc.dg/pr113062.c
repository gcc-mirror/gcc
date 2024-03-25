/* { dg-do compile } */
/* { dg-options "-Oz -fharden-conditional-branches" } */
long double foo;
double bar;
void abort();
void check() {
  if (foo == bar)
    abort();
}

