/* { dg-do compile { target { ! logical_op_short_circuit  } } } */
/* { dg-options "-O2 -fdump-tree-dom2-details -w" } */
unsigned char
validate_subreg (unsigned int offset, unsigned int isize, unsigned int osize, int zz, int qq)
{
if (osize >= (((zz & (1L << 2)) != 0) ? 8 : 4) && isize >= osize)
    ;
  else if (qq == 99)
 return 0;
  if (osize > isize)
    return offset == 0;
  return 1;
}
/* When we test isize >= osize in the first IF conditional and it is
   false and qq != 99, then we can thread the osize > isize test of
   the second conditional.  */
/* { dg-final { scan-tree-dump-times "Threaded" 1 "dom2"} } */
