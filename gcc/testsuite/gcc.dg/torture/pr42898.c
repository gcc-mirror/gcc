/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */

struct hardware {
  int parm1:8;
  int :4;
  int parm2:4;
  int parm3:15;
  int parm4:1;
};

void f1(volatile struct hardware *ptr)
{
  *ptr=(struct hardware) {
    .parm1=42,
    .parm2=13,
    .parm3=11850,
    .parm4=1,
  };
}

/* { dg-final { scan-tree-dump-times "\\*ptr" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
