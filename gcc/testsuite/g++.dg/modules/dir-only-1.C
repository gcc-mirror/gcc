// { dg-do preprocess }
// { dg-additional-options -fdirectives-only }

#define major NO NOT ME

#ifdef major
#  undef major
#else
#  error major not initially defined
#endif

#ifdef major
#  error major still defined
#endif

// { dg-final { scan-file dir-only-1.i "#undef major\n" } }
