/* { dg-do run } */
#include <assert.h>

static inline int
blk_oversized_queue (int* q)
{
  if (q[2])
    return q[1] != 0;
  return q[0] == 0;
}

int __attribute__ ((noinline))
get_request (int* q, int rw)
{
  if (blk_oversized_queue (q))
    {
      if ((rw == 1) || (rw == 0))
	return -33;

      return 0;
    }

  return -100;
}

int main (void)
{
  int x[3]; 
  int r;

  x[0] = 0; x[1] = 1; x[2] = 1;
  r = get_request (x, 0);
  assert (r == -33);

  r = get_request (x, 1);
  assert (r == -33);

  r = get_request (x, 2);
  assert (r == 0);


  x[0] = 0; x[1] = 0; x[2] = 1;
  r = get_request (x, 0);
  assert (r == -100);

  r = get_request (x, 1);
  assert (r == -100);

  r = get_request (x, 2);
  assert (r == -100);


  x[0] = 0; x[1] = 0; x[2] = 0;
  r = get_request (x, 0);
  assert (r == -33);

  r = get_request (x, 1);
  assert (r == -33);

  r = get_request (x, 2);
  assert (r == 0);


  x[0] = 0; x[1] = 0; x[2] = 0;
  r = get_request (x, 0);
  assert (r == -33);

  r = get_request (x, 1);
  assert (r == -33);

  r = get_request (x, 2);
  assert (r == 0);

  return 0;
}
