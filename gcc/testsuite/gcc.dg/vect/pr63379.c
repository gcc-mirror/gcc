/* PR tree-optimization/63379  */

#include "tree-vect.h"

extern void abort (void);

typedef struct {
    int x;
    int y;
} Point;

Point pt_array[25];

void __attribute__((noinline,noclone))
generate_array(void)
{
  unsigned int i;
  for (i = 0; i<25; i++)
    {
      pt_array[i].x = i;
      pt_array[i].y = 1000+i;
    }
}

int main()
{
  check_vect ();
  generate_array ();
  Point min_pt = pt_array[0];
  Point *ptr, *ptr_end;
  for (ptr = pt_array+1, ptr_end = pt_array+25; ptr != ptr_end; ++ptr)
    {
      min_pt.x = (min_pt.x < ptr->x) ? min_pt.x : ptr->x;
      min_pt.y = (min_pt.y < ptr->y) ? min_pt.y : ptr->y;
    }

  if (min_pt.x != 0 || min_pt.y != 1000)
    abort ();
  return 0;
}

