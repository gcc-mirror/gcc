/* Tests for unterminated conditionals: 3.  */

#if 1  /* { dg-error "#else" "unterminated #else" } */
#else
