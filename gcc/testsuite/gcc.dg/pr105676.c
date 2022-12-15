/* { dg-do compile } */
/* { dg-options "-O2 -Wsuggest-attribute=pure" } */

__attribute__((const))
extern int do_expensive_calculation(void);

__attribute__((const))
int getval(void) /* { dg-bogus "candidate for attribute" } */
{
  static int cache = -1;
  if (cache == -1)
    cache = do_expensive_calculation();
  return cache;
}
