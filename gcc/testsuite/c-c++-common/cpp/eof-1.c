/* PR preprocess/95183  */

/* { dg-do preprocess } */

#define f(x) x

f( /* { dg-error "-:unterminated" "unterminated macro" } */
