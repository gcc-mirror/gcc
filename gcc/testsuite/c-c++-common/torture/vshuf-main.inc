/* Driver fragment for __builtin_shuffle of any vector shape.  */

extern void abort(void);

int main()
{
#ifndef UNSUPPORTED
  int i;

  for (i = 0; i < sizeof(tests)/sizeof(tests[0]); ++i)
    {
      V r = __builtin_shuffle(tests[i].in, tests[i].mask);
      if (__builtin_memcmp(&r, &tests[i].out, sizeof(V)) != 0)
	abort();
    }

  for (i = 0; i < sizeof(tests2)/sizeof(tests2[0]); ++i)
    {
      V r = __builtin_shuffle(tests2[i].in1, tests2[i].in2, tests2[i].mask);
      if (__builtin_memcmp(&r, &tests2[i].out, sizeof(V)) != 0)
	abort();
    }
#endif

  return 0;
}
