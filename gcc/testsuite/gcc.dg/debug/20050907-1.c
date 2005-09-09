/* PR 23190 */
/* Well, collateral damage from a proposed patch fixing 23190.  We'd emit
   debug info for multilib_exclusions_raw without emitting the variable
   itself, leading to link errors.  This reduced form is filed as PR 23777,
   for not eliminating things soon enough.  */
/* { dg-do link } */

static const char *const multilib_exclusions_raw[] = { 0 };

void __attribute__((noinline)) f(char *const *p)
{
  __asm__ ("" : : "g"(p) : "memory");
}

void g (char **o)
{
  const char *const *q = multilib_exclusions_raw;

  f (o);
  while (*q++)
    f (o);
}

int main() { return 0; }
