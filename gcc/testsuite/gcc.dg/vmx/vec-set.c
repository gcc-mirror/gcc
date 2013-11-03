#include "harness.h"

vector short
vec_set (short m)
{
  return (vector short){m, 0, 0, 0, 0, 0, 0, 0};
}

static void test()
{
  check (vec_all_eq (vec_set (7),
		     ((vector short){7, 0, 0, 0, 0, 0, 0, 0})),
	 "vec_set");
}
