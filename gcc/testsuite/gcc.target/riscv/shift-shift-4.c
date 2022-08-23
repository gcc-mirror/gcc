/* { dg-do compile } */
/* { dg-options "-march=rv32i -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */

/* One zero-extend shift can be eliminated by modifying the constant in the
   greater than test.  Started working after modifying the splitter
   lshrsi3_zero_extend_3+1 to use a temporary reg for the first split dest.  */
int
sub (int i)
{
  i &= 0x7fffffff;
  return i > 0x7f800000;
}
/* { dg-final { scan-assembler-not "srli" } } */
