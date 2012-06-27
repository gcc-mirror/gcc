/* Driver fragment for __builtin_shuffle of any vector shape.  */

// { dg-options "-std=c++11" }
// { dg-do run }

extern "C" void abort(void);

int main()
{
#ifndef UNSUPPORTED
  int i;

  for (i = 0; i < sizeof(in1)/sizeof(in1[0]); ++i)
    {
      V r = __builtin_shuffle(in1[i], mask1[i]);
      if (__builtin_memcmp(&r, &out1[i], sizeof(V)) != 0)
	abort();
    }

  for (i = 0; i < sizeof(in2)/sizeof(in2[0]); ++i)
    {
      V r = __builtin_shuffle(in2[i], in3, mask2[i]);
      if (__builtin_memcmp(&r, &out2[i], sizeof(V)) != 0)
	abort();
    }
#endif

  return 0;
}
