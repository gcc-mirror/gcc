/* { dg-do preprocess } */
/* { dg-options "-std=c++98 -fpermissive" } */

#if 1   
#endif 1 /* { dg-warning "warning: extra tokens at end of #endif directive" } */
