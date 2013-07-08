/* PR target/57819 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */

void foo (void);

__extension__ typedef __INTPTR_TYPE__ intptr_t;

int
test1 (intptr_t x, intptr_t n)
{
  n &= sizeof (intptr_t) * __CHAR_BIT__ - 1;

  if (x & ((intptr_t) 1 << n))
    foo ();

  return 0;
}

int
test2 (intptr_t x, intptr_t n)
{
  if (x & ((intptr_t) 1 << ((int) n & (sizeof (intptr_t) * __CHAR_BIT__ - 1))))
    foo ();

  return 0;
}

int
test3 (intptr_t x, intptr_t n)
{
  if (x & ((intptr_t) 1 << ((int) n & ((int) sizeof (intptr_t) * __CHAR_BIT__ - 1))))
    foo ();

  return 0;
}

/* { dg-final { scan-assembler-not "and\[lq\]\[ \t\]" } } */
