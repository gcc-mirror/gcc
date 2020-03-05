/* PR c/90737 - inconsistent address of a local converted to intptr_t
   between callee and caller
   { dg-do compile }
   { dg-options "-O1 -Wall -Wreturn-local-addr -fdump-tree-optimized" } */

typedef __INTPTR_TYPE__ intptr_t;

const intptr_t&
return_addr_label_as_intref (void)
{
 label:
  if ((const intptr_t*)&&label == 0)
    __builtin_exit (1);

  return *(const intptr_t*)&&label;   // { dg-warning "\\\[-Wreturn-local-addr]" } */
}

const intptr_t&
return_addr_local_as_intref (void)
{
  int a[1];
  if ((const intptr_t*)a == 0)
    __builtin_exit (1);

  return (const intptr_t&)a;   // { dg-warning "\\\[-Wreturn-local-addr]" } */
}

/* Verify that the return value has been replaced with zero:
  { dg-final { scan-tree-dump-times "return 0;" 2 "optimized" } } */
