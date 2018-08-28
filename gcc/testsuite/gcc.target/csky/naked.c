/* { dg-do compile } */
/* { dg-final { scan-assembler-not "push" } } */
/* { dg-final { scan-assembler-not "pop" } } */

/* Check that there is no prologue/epilogue code emitted for a function
   with the naked attribute.  Without the attribute, this function would
   push/pop lr.  */

extern void g (int);

int __attribute__((naked))
f (int x)
{
  g (x);
  return 42;
}
