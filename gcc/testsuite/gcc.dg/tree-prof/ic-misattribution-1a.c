/* { dg-options "-DEMPTY" } */
/* This file is only needed in combination with ic-misattribution-1.c
   but there's no easy way to make this file ignored. */
extern void callee (void);
extern void caller (void (*func) (void));

typedef void (*func_t) (void);
func_t func;

int
main ()
{
#ifdef EMPTY
#else
  func = callee;
  caller (callee);
  func ();
#endif
  return 0;
}

