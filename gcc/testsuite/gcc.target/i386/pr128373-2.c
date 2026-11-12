/* PR target/128373 */
/* { dg-do preprocess { target *-*-linux* } } */
/* { dg-options "-fcf-protection=full -fcf-protection=check -march=x86-64" } */

#if __CET__ != 3
# error "__CET__ must be 3 with -fcf-protection=full"
#endif
