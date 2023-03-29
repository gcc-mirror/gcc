#define NULL ((void*)0)

#define MY_ASSERT(COND)				\
  do {						\
    if (!(COND)) { __builtin_abort(); }		\
  } while (0)

int test_1 (int *p)
{
  int result = *p;
  MY_ASSERT (p); /* { dg-warning "check of 'p' for NULL after already dereferencing it" "" { xfail *-*-* } } */
  /* Due to lack of locations for gimple arguments we can't get
     at the location of the condition separately from the
     gimple_cond stmt, and thus can't distinguish if it's in the
     macro definition or in the supplied params; we defer to
     rejecting the diagnostic.  */
  return result;
}

int test_2 (int *p)
{
  int result = *p;
  MY_ASSERT (p != NULL); /* { dg-warning "check of 'p' for NULL after already dereferencing it" "" { xfail *-*-* } } */
  return result;
}
