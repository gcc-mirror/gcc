/* This tests whether non-offsettable memory operands are reloaded 
   correctly in certain corner cases on s390 targets.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

void test_inout (char *bd, int xd, char *bs, int xs)
{
  *(long long *)(bd + xd + 4093) = *(long long *)(bs + xs + 4093);
}

void test_in (char *bd, int xd, char *bs, int xs)
{
  *(long long *)(bd + xd) = *(long long *)(bs + xs + 4093);
}

void test_out (char *bd, int xd, char *bs, int xs)
{
  *(long long *)(bd + xd + 4093) = *(long long *)(bs + xs);
}

