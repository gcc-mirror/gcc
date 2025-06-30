/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-expand" } */

void s8(signed char c);
void cs8(signed char *p)
{
  s8(*p);
}

/* { dg-final { scan-rtl-dump "sign_extend" "expand" } } */
/* { dg-final { scan-rtl-dump-not "zero_extend" "expand" } } */
