/* PR debug/88635 */
/* { dg-do assemble } */
/* { dg-options "-g -O2" } */
/* { dg-additional-options "-fpie" { target pie } } */

static void
foo (char *b)
{
  unsigned c = 0;
  --c;
  do
    if (++*b++ == 0)
      break;
  while (--c);
  if (c == 0)
    while (*b++)
      ;
}

void
bar (void)
{
  foo ("");
}
