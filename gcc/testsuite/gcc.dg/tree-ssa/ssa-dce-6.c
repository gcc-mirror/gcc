/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

struct object { int field; };
void o(struct object *);
int globl;
void t(int x)
{
  struct object a, b;
  struct object *p;
  o(&a);
  if (x)
    p = &a;
  else
    p = &b;
  p->field = 1;
  globl = 0;
  return;
}

/* The global store should not prevent deleting the store to p->field.  */

/* { dg-final { scan-tree-dump-not "p_.->field" "cddce1" } } */
/* { dg-final { cleanup-tree-dump "cddce1" } } */
