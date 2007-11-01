/* PR preprocessor/30805 - ICE while token pasting.  */
/* { dg-do preprocess } */

#define A(x,...) x##,##__VA_ARGS__
A(1)
