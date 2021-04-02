/* PR sanitizer/98204 */
/* { dg-options "-fsanitize=address,pointer-subtract,pointer-compare" } */

struct{int c;}v;
static long i=((char*)&(v.c)-(char*)&v);
static long i2=((char*)&(v.c)<(char*)&v);
