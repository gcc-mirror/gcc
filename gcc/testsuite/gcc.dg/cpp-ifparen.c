/* { dg-do preprocess } */

#if 2048 < (16 * (40) + 192)
#error			/* { dg-bogus "error" "with paren" } */
#endif

#if 2048 < (16 * 40 + 192)
#error			/* { dg-bogus "error" "without paren" } */
#endif
