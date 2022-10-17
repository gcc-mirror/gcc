/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=any,ucn" } */
/* Test we ignore UCNs in comments.  */

// a b c \u202a 1 2 3
// a b c \u202A 1 2 3
/* a b c \u202a 1 2 3 */
/* a b c \u202A 1 2 3 */
