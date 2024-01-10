/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target bitint } */

_BitInt(998) b;
char c;
char d;
char e;
char f;
char g;
char h;
char i;
char j;

void
foo(char y, _BitInt(9020) a, char *r)
{
  char x = __builtin_mul_overflow_p(a << sizeof(a), y, 0);
  x += c + d + e + f + g + h + i + j + b;
  *r = x;
}

int
main(void)
{
  char x;
  foo(5, 5, &x);
  if (x != 1)
    __builtin_abort();
  return 0;
}
