/* PR target/114696 */
/* { dg-do assemble { target { apxf && { ! ia32 } } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-mapxf -O0 -mx32" } */

_Thread_local unsigned _BitInt(65) a;
_BitInt(129) b;

void
foo (void)
{
  a += b;
}
