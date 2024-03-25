#include "analyzer-decls.h"

struct coord
{
  int x;
  int y;
};

void test_1 (void)
{
  struct coord arr[16];

  arr[2].y = 4;
  arr[3].x = 5;
  arr[3].y = 6;
  arr[4].x = 7;
  arr[6].y = 8;
  arr[8].x = 9;

  arr[7] = arr[3];

  __analyzer_eval (arr[7].x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[7].y == 6); /* { dg-warning "TRUE" } */

  /* Make sure we don't touch the neighbors.  */
  __analyzer_eval (arr[6].y == 8); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[8].x == 9); /* { dg-warning "TRUE" } */
}
