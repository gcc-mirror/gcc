/* PR target/128373 */
/* { dg-do preprocess { target *-*-linux* } } */
/* { dg-options "-fcf-protection=check -march=x86-64" } */

#ifdef __CET__
# error "__CET__ must not be defined with -fcf-protection=check"
#endif
