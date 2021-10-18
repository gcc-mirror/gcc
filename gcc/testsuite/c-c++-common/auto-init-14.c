/* Verify the auto initialization of structure or union with a flexible array
   member.  */ 
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-tree-gimple" } */

struct a {
  int b;
  int array[];
};
union tar {
  struct a bar;
  char buf;
};

int foo()
{
  struct a d;
  union tar var;
  return d.b + var.bar.b;
}

/* { dg-final { scan-tree-dump "d = .DEFERRED_INIT \\(4, 2, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "var = .DEFERRED_INIT \\(4, 2, 0\\)" "gimple" } } */
