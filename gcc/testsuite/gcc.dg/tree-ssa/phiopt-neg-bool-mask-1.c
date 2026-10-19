/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt" } */

int
direct_negative_mask (int outer, int value)
{
  int mask = -(value > 0);
  return outer > 0 ? mask : 0;
}

int
direct_negative_mask_minus_one (int outer, int value)
{
  int mask = -(value > 0);
  return outer > 0 ? mask : -1;
}

unsigned int
direct_negative_mask_unsigned (int outer, int value)
{
  unsigned int mask = -(unsigned int) (value > 0);
  return outer > 0 ? mask : 0;
}

unsigned int
direct_negative_mask_minus_one_unsigned (int outer, int value)
{
  unsigned int mask = -(unsigned int) (value > 0);
  return outer > 0 ? mask : -1U;
}

/* This happen in phiopt2 but currently this depends on the
   sink pass to make operations conditional. */

/* The zero else arms contribute two AND operations.  */
/* { dg-final { scan-tree-dump-times " & " 2 "phiopt4" } } */
/* { dg-final { scan-tree-dump-times " & " 2 "phiopt2" { xfail *-*-* } } } */
/* The minus-one else arms contribute two OR operations.  */
/* { dg-final { scan-tree-dump-times " \\| " 2 "phiopt2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times " \\| " 2 "phiopt4" } } */
/* { dg-final { scan-tree-dump-not "if \\(" "phiopt2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not "if \\(" "phiopt4" } } */
