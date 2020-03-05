/* PR c/90737 - inconsistent address of a local converted to intptr_t
   between callee and caller
   { dg-do compile }
   { dg-options "-O1 -Wall -Wreturn-local-addr -fdump-tree-optimized" } */

typedef __INTPTR_TYPE__ intptr_t;

static inline intptr_t
return_addr_local_as_int (void)
{
  int i;
  if ((intptr_t)&i == 0)
    __builtin_abort ();

  return (intptr_t)&i;
}

void get_addr_local_as_int (void)
{
  intptr_t i = return_addr_local_as_int ();
  if (i == 0)
    __builtin_abort ();
}


static inline intptr_t
return_addr_label_as_int (void)
{
 label:
  if ((intptr_t)&&label == 0)
    __builtin_abort ();

  return (intptr_t)&&label;
}

void get_addr_label_as_int (void)
{
  intptr_t i = return_addr_label_as_int ();
  if (i == 0)
    __builtin_abort ();
}

/* Verify that the functions that return the address of the label
   or local variable have been optimized away and so have the calls
   to abort.
  { dg-final { scan-tree-dump-not "return_addr_" "optimized" } }
  { dg-final { scan-tree-dump-not "abort" "optimized" } } */
