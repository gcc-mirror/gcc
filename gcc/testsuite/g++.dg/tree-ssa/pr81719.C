/* { dg-do compile { target c++11 } } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

typedef int Items[2];

struct ItemArray
{
  Items items;
  int sum_x2() const;
};

int ItemArray::sum_x2() const
{
  int total = 0;
  for (int item : items)
    {
      total += item;
    }
  return total;
}

/* We should be able to compute the number of iterations to two, unroll
   the loop and end up with a single basic-block in sum_x2.  */
/* { dg-final { scan-tree-dump-times "bb" 1 "optimized" } } */
