/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

struct hardware {
  int parm1:8;
  int :4;
  int parm2:4;
  int parm3:15;
  int parm4:1;
};

const struct hardware h = {
  .parm1=42,
  .parm2=13,
  .parm3=11850,
  .parm4=1,
};

void f1(volatile struct hardware *ptr)
{
  *ptr = h;
}

/* { dg-final { scan-tree-dump-times "\\*ptr" 1 "optimized" } } */
