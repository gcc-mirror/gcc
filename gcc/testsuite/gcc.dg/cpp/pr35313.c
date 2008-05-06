/* Test two failing cases for libcpp parser.  From PRs 35313, 36088*/
/* { dg-do preprocess } */
/* { dg-options "-std=c99 -pedantic-errors" } */

extern int x;

#if 0 ? 3,4 : 2
#endif

#if 1 ? 0 : 1 ? 1/0 : 1/0
#endif
