/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1" } */

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
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 3 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 3 "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if " "phiopt1" } } */
