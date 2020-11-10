/* Test we do correct thing for adding to / subtracting from a pointer,
   i.e. that the multiplication by the size of the pointer target type
   still occurs.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */
/* { dg-xfail-run-if "PR97444: stack atomics" { nvptx*-*-* } }*/

#define TEST_POINTER_ADD_SUB(TYPE)			\
  do							\
    {							\
      TYPE a[3][3];					\
      TYPE (*_Atomic q)[3] = &a[0];			\
      ++q;						\
      if (q != &a[1])					\
	__builtin_abort ();				\
      q++;						\
      if (q != &a[2])					\
	__builtin_abort ();				\
      --q;						\
      if (q != &a[1])					\
	__builtin_abort ();				\
      q--;						\
      if (q != &a[0])					\
	__builtin_abort ();				\
      q += 2;						\
      if (q != &a[2])					\
	__builtin_abort ();				\
      q -= 2;						\
      if (q != &a[0])					\
	__builtin_abort ();				\
    }							\
  while (0)

int
main (void)
{
  TEST_POINTER_ADD_SUB (_Bool);
  TEST_POINTER_ADD_SUB (char);
  TEST_POINTER_ADD_SUB (signed char);
  TEST_POINTER_ADD_SUB (unsigned char);
  TEST_POINTER_ADD_SUB (signed short);
  TEST_POINTER_ADD_SUB (unsigned short);
  TEST_POINTER_ADD_SUB (signed int);
  TEST_POINTER_ADD_SUB (unsigned int);
  TEST_POINTER_ADD_SUB (signed long);
  TEST_POINTER_ADD_SUB (unsigned long);
  TEST_POINTER_ADD_SUB (signed long long);
  TEST_POINTER_ADD_SUB (unsigned long long);
  TEST_POINTER_ADD_SUB (float);
  TEST_POINTER_ADD_SUB (double);
  TEST_POINTER_ADD_SUB (long double);
  TEST_POINTER_ADD_SUB (_Complex float);
  TEST_POINTER_ADD_SUB (_Complex double);
  TEST_POINTER_ADD_SUB (_Complex long double);
}
