/* PR target/98086 */
/* { dg-do compile } */
/* { dg-options "" } */

#ifdef __x86_64__
typedef __int128 T;
#else
typedef long long T;
#endif

T x;

void
foo (void)
{
  __asm ("" : "=@ccc" (x));
}
