/* Test -undef.  A bug in its handling broke glibc builds because
   "linux" was wrongly defined.  */
/* { dg-do preprocess } */
/* { dg-options "-undef" } */

#ifdef linux
#error -undef broken
#endif
