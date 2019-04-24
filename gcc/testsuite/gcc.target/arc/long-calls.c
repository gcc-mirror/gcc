/* { dg-do compile } */
/* { dg-options "-O2 -mlong-calls" } */

int g (void);

int f (void)
{
  g();
}

/* { dg-final { scan-assembler "j\\t@g" } } */
