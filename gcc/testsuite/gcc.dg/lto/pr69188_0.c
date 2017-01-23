/* PR ipa/69188 */
/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -O0 -fprofile-generate } } } */
/* { dg-require-profiling "-fprofile-generate" } */

void fn1(void)
{
}
