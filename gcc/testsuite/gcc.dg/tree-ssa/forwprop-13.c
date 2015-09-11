/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

struct B {
    unsigned bit0 : 1;
    unsigned bit1 : 1;
};

void
foo (struct B *b)
{
  b->bit0 = b->bit0 | b->bit1;
}

/* { dg-final { scan-tree-dump-not "\\\(unsigned" "optimized" } } */
