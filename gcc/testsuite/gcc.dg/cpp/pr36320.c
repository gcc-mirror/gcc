/* PR 36320 - #elif still requires valid expression.  */

/* { dg-do preprocess } */

int z;
#if 1
#elif   /* { dg-error "with no expression" } */
#endif
