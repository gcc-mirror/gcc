/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt -fdump-tree-evrp" } */

int foo_add (const unsigned char *tmp, int i, int val)
{
  return (unsigned char)(((tmp[i] + val)>0xFF)?0xFF:(((tmp[i] + val)<0)?0:(tmp[i] + val)));
}

int foo_sub (const unsigned char *tmp, int i, int val)
{
  return (unsigned char)(((tmp[i] - val)>0xFF)?0xFF:(((tmp[i] - val)<0)?0:(tmp[i] - val)));
}

int foo_mul (const unsigned char *tmp, int i, int val)
{
  return (unsigned char)(((tmp[i] * val)>0xFF)?0xFF:(((tmp[i] * val)<0)?0:(tmp[i] * val)));
}

/* All cases should end up using min/max for the saturated operations and
   have no control flow.  */
/* EVRP leaves copies in the IL which confuses phiopt1 so we have
   to rely on phiopt2 instead.  */
/* { dg-final { scan-tree-dump-not " & 255;" "evrp" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 3 "phiopt1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 3 "phiopt1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not "if " "phiopt1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 3 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 3 "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "if " "phiopt2" } } */
