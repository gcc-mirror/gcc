/* A member pointing to a tagged structure in a named address space must
   not make the canonical type of the enclosing structure ill-formed.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -O2" } */

struct S { int x; };

struct C { struct S __seg_gs *p; };
struct D { struct S __seg_fs **pp; const struct S __seg_gs *cp; };

int
f (struct C *c)
{
  return c->p->x;
}

/* { dg-final { scan-assembler "%gs:" } } */
