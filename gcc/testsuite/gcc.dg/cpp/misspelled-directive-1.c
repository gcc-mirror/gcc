#ifndef SOME_GUARD /* { dg-error "-:unterminated" } */

#if 1
/* Typo here: "endfi" should have been "endif".  */
#endfi /* { dg-error "invalid preprocessing directive #endfi; did you mean #endif?" } */

int make_non_empty;

/* Another transposition typo:  */
#deifne FOO /* { dg-error "invalid preprocessing directive #deifne; did you mean #define?" } */ 

#endif /* #ifndef SOME_GUARD */
