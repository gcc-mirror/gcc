/* { dg-do preprocess } */

/* Test source Robert Lipe, with minor modifications for the testsuite
   by Neil Booth.  29 Oct 2000.  */

#define _VA_ARGS_0() 42
#define _L_0()                      (
#define _R_0()                      )

#define __VA_ARGLIST(argc,list) \
                _VA_ARGS_##argc list

#define _CAT_LIST(argc,list1,list2) \
                _L_##argc list1 _R_##argc list2

#define _VA_ARGLIST(argc,list1,list2) \
                __VA_ARGLIST(argc, \
                        _CAT_LIST(argc, list1, list2))

#define BLAH(a) _VA_ARGLIST(a, (), ())

#if BLAH (0) != 42
#error Simulated varargs macros
#endif
