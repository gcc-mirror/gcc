/* PR 36320 - #elif still requires valid expression.  */
/* DR#412: #elif doesn't have to be valid expression (PR60570).  */
/* { dg-do preprocess } */

int z;
#if 1
#elif	/* { dg-bogus "with no expression" } */
#endif
