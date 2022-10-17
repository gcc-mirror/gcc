/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

unsigned __int128 u_div(unsigned __int128 a, unsigned __int128 b)
{
   return a/b;
}

unsigned __int128 u_mod(unsigned __int128 a, unsigned __int128 b)
{
   return a%b;
}
__int128 s_div(__int128 a, __int128 b)
{
   return a/b;
}

__int128 s_mod(__int128 a, __int128 b)
{
   return a%b;
}

/* { dg-final { scan-assembler {\mvdivsq\M} } } */
/* { dg-final { scan-assembler {\mvdivuq\M} } } */
/* { dg-final { scan-assembler {\mvmodsq\M} } } */
/* { dg-final { scan-assembler {\mvmoduq\M} } } */
