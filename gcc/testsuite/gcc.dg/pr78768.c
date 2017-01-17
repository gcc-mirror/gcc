/* PR c/78768 - -Walloca-larger-than and -Wformat-overflow warnings disabled
   by -flto
  { dg-do link }
  { dg-require-effective-target lto }
  { dg-options "-O2 -Walloca-larger-than=10 -Wformat -Wformat-overflow -flto" } */

int main (void)
{
  char *d = (char *)__builtin_alloca (12);  /* { dg-warning "argument to .alloca. is too large" } */

  __builtin_sprintf (d, "%32s", "x");   /* { dg-warning "directive writing 32 bytes into a region of size 12" "-Wformat-overflow" { xfail *-*-* } } */

  return 0;
}
