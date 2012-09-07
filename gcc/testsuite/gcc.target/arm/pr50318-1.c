/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long test (unsigned int sec, unsigned long long nsecs)
{
   return (long long)(long)sec * 1000000000L + (long long)(unsigned
   long)nsecs;
}

/* { dg-final { scan-assembler "smlal" } } */
