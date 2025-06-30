/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-expand" } */

void u8(unsigned char c);
void cu8(unsigned char *p)
{
  u8(*p);
}

/* { dg-final { scan-rtl-dump "zero_extend" "expand" } } */
/* { dg-final { scan-rtl-dump-not "sign_extend" "expand" } } */
