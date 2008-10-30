/* This used to ICE on s390 due to bug in the definition of the 'R'
   constraint which replaced the 'm' constraint (together with 'T')
   while adding z10 support.  */

/* { dg-do compile } */
/* { dg-options "-O" } */

int boo()
{
  struct {
    unsigned char pad[4096];
    unsigned long bar;
  } *foo;
  asm volatile( "" : "=m" (*(unsigned long long*)(foo->bar))
		: "a" (&foo->bar));
}
