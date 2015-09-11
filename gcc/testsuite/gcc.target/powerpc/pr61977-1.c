/* PR preprocessor/61977 */
/* { dg-do preprocess } */
/* { dg-options "-mno-altivec -mno-vsx" } */

int y; vector
int x;

/* { dg-final { scan-file "pr61977-1.i" "(^|\\n)int y; vector\\n" } } */
