/* { dg-do compile } */
/* { dg-options "-O2" } */
void *a;
void *b;
void
t(int c)
{
  if (c<10)
    __builtin_memcpy (a,b,c);
}
/* Memcpy should be inlined because block size is known.  */
/* { dg-final { scan-assembler-not "(jmp|call)\[\\t \]*memcpy" } } */
