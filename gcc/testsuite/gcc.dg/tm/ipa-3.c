/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

static int x;

void __attribute__((transaction_callable))
foo(void)
{
  x++;
}

/* { dg-final { scan-assembler "_ZGTt3foo" } } */
