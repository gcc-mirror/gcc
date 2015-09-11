/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-times "sh.add" 1 } }  */

typedef struct
{
  unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
}
__sigset_t;
int
__sigaddset (__sigset_t * __set, int __sig, int __stuff)
{
  unsigned long int __word =
    (((__sig) - 1) / (8 * sizeof (unsigned long int)));
  return __set->__val[__word] = __stuff;
}
