/* { dg-do run } */

extern void abort (void);

#define LEN 4

static inline void unpack(int  array[LEN])
{
  int ii, val;
  val = 1;
  for (ii = 0; ii < LEN; ii++) {
      array[ii] = val % 2;
      val = val / 2;
  }
}

static inline int  pack(int  array[LEN])
{
  int ans, ii;
  ans = 0;
  for (ii = LEN-1; ii >= 0; ii--) {
      ans = 2 * ans + array[ii];
  }
  return ans;
}

int __attribute__((noinline))
foo()
{
  int temp, ans;
  int array[LEN];
  unpack(array);
  temp = array[0];
  array[0] = array[2];
  array[2] = temp;
  ans = pack(array);
  return ans;
}

int main(void)
{
  int val;
  val = foo();
  if (val != 4)
    abort ();
  return 0;
}
