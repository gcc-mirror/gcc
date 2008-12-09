/* { dg-options "-O3 -fgraphite-identity" } */

typedef struct {
  unsigned int avail_out;
  void *state;
} stream;

typedef struct {
  stream* test;
  int num;
} state_in;

int test_in ( stream *test, int action )
{
  state_in* tst;
  if (test == ((void *)0)) return (-2);
  if (tst == ((void *)0)) return (-2);
  if (tst->test != test) return (-2);
 jump_here:
  switch (tst->num) {
  case 1:
    return (-1);
  case 2:
    if (action == 0) {
    }
    if (action == 1) {
      goto jump_here;
    }
  }
  return 0;
}
