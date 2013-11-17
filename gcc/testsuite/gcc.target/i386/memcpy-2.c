/* { dg-do compile } */
/* { dg-options "-O2" } */
/* Memcpy should be inlined because block size is known.  */
/* { dg-final { scan-assembler-not "memcpy" } } */
void *a;
void *b;
t(unsigned int c)
{
  if (c<10)
    memcpy (a,b,c);
}
