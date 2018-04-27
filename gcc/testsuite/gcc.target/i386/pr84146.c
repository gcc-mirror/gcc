/* PR target/84146 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -fcf-protection=full -fcompare-debug" } */

int __setjmp (void **);
void *buf[64];

void
foo (void)
{
  __setjmp (buf);
  for (;;)
    ;
}
