/* Tests for unterminated conditionals: 3.  */
/* { dg-do preprocess } */

#if 1  /* { dg-error "#else" "unterminated #else" } */
#else
