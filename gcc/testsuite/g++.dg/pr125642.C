/* { dg-do preprocess } */

#ifdef __FRACT_FBIT__
#error "we should not report _Fract support in C++"
#endif
