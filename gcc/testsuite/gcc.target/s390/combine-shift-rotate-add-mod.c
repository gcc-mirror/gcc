/* Check shift via address-style displacement.  There should not be any
   and operations that the instructions perform implicitly anyway.*/

/* { dg-options "-O1 -m64" } */

/* { dg-final { scan-assembler-not "risbg\t%r.+,.*63" } } */
/* { dg-final { scan-assembler "rllg\t%r.+,3.%r.+" } } */
/* { dg-final { scan-assembler "sllg\t%r.+,2.%r.+" } } */

unsigned long rotlmodp (unsigned long in, unsigned long sh)
{
   sh = (sh + 3) % 64;
   return (in << sh) | (in >> (64 - sh));
}

unsigned long shiftmodp (unsigned long in, unsigned long sh)
{
   sh = (sh + 2) % 64;
   return (in << sh);
}

/* We expect a displacement of 1 here since combine simplifies
   modulo 255 when substituting into a QImode subreg.  */
/* { dg-final { scan-assembler "sllg\t%r.+,1.%r.+" } } */
unsigned long shiftp (unsigned long in, unsigned long sh)
{
   sh = sh + 4097;
   return (in << sh);
}
