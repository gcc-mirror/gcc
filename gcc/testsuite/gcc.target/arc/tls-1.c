/* { dg-do compile } */
/* { dg-require-effective-target tls } */
/* { dg-skip-if "" { arc*-*-elf* } } */
/* { dg-options "-O3 -std=gnu99" } */

/* Check if addressing the `pos` member of struct is done via tls
   mechanism.  */

struct callchain_cursor
{
  int last;
  long long pos;
} __thread a;

void fn1 (struct callchain_cursor *p1)
{
  p1->pos++;
}

extern void fn3 (void);

void fn2 (void)
{
  struct callchain_cursor *b = &a;
  while (1)
    {
      fn3 ();
      fn1 (b);
    }
}
/* { dg-final { scan-assembler "r25,@a@tpoff" } } */
