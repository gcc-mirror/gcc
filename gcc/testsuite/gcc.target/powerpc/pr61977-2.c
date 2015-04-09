/* PR preprocessor/61977 */
/* { dg-do preprocess } */
/* { dg-options "-maltivec" } */

int y; vector
int x;

/* { dg-final { scan-file "pr61977-2.i" "(^|\\n)int y; __attribute__\\(\\(altivec\\(vector__\\)\\)\\)\\n" } } */
