/* { dg-do compile }  */
/* { dg-options "-fdump-tree-evrp -std=c++11 -fmath-errno -fno-exceptions -O3 -W -Wall" } */

  struct type {
        unsigned long long t;
        int t1;
  } ;
struct a
{
  type per_lane_size_states[16];
}TestForGEVectorsState;

void sink(int);

static constexpr int kMaxSupportedLaneSize = 8;

void dead();

void f()
{
  for (int lane_size = 1; lane_size <= kMaxSupportedLaneSize; lane_size <<= 1) {
    type *tp = &TestForGEVectorsState.per_lane_size_states[lane_size];
    if (lane_size < 0)
      dead ();
    if ((unsigned long long)(lane_size * 8) <= 64llu)
    {
      unsigned long long t = lane_size;
      t = 24 / t;
      if (tp->t != t)
      {
        __builtin_trap();
      }
    }
    else  if (tp->t1)
    __builtin_trap();
  }
}

/* { dg-final { scan-tree-dump-not "dead" "evrp" } }  */
