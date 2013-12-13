/* Test C11 _Alignof returning minimum alignment for a type.  PR
   52023.  */
/* { dg-do run } */
/* { dg-options "-std=c11" } */

extern void abort (void);
extern void exit (int);

#define CHECK_ALIGN(TYPE)			\
  do						\
    {						\
      struct { char c; TYPE v; } x;		\
      if (_Alignof (TYPE) > __alignof__ (x.v))	\
	abort ();				\
    }						\
  while (0)

int
main (void)
{
  CHECK_ALIGN (_Bool);
  CHECK_ALIGN (char);
  CHECK_ALIGN (signed char);
  CHECK_ALIGN (unsigned char);
  CHECK_ALIGN (signed short);
  CHECK_ALIGN (unsigned short);
  CHECK_ALIGN (signed int);
  CHECK_ALIGN (unsigned int);
  CHECK_ALIGN (signed long);
  CHECK_ALIGN (unsigned long);
  CHECK_ALIGN (signed long long);
  CHECK_ALIGN (unsigned long long);
  CHECK_ALIGN (float);
  CHECK_ALIGN (double);
  CHECK_ALIGN (long double);
  CHECK_ALIGN (_Complex float);
  CHECK_ALIGN (_Complex double);
  CHECK_ALIGN (_Complex long double);
  exit (0);
}
