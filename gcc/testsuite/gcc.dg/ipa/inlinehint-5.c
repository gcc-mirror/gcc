/* { dg-options "-O2 -fdump-ipa-inline-details -fno-early-inlining " } */
/* { dg-add-options bind_pic_locally } */
int j,k,l;
int test3(int);
int test4(int);

static inline int
test2(int i)
{
  if (__builtin_constant_p (i))
    {
	switch (i)
	{
	case 1: return j;
	case 2: return k;
	case 3: return l;
	}
    }
  else return test3(i)+test4(i);
}

static inline int
test (int i)
{
  return test2(i) + test2(i+1) + test3 (i) + test3(i) + test3(i) + test3 (i);
}

int
run (int i)
{
   return test (i) + test (i);
}
/* The test should work by first inlining test2->test and then test to run
   Both are called twice, so 4 hints (the second make sure that we propagate
   to callers.  */
/* { dg-final { scan-ipa-dump-times "hints: declared_inline builtin_constant_p" 4 "inline"  } } */
