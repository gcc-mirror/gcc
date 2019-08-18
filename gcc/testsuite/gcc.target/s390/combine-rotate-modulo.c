/* Check that we do not emit & 63 via risbg for rotating.  */

/* { dg-options "-O1 -m64" } */

/* { dg-final { scan-assembler-not "risbg" } } */
/* { dg-final { scan-assembler-not "nilf" } } */

long shiftl (long in, unsigned long sh)
{
   sh %= 64;
   return (in << sh);
}

unsigned long shiftll (unsigned long in, unsigned long sh)
{
   sh %= 64;
   return (in << sh);
}

long shiftr (long in, unsigned long sh)
{
   sh %= 64;
   return (in >> sh);
}

unsigned long shiftrl (unsigned long in, unsigned long sh)
{
   sh %= 64;
   return (in >> sh);
}

unsigned long rotlmod (unsigned long in, unsigned long sh)
{
   sh %= 64;
   return (in << sh) | (in >> (64 - sh));
}
