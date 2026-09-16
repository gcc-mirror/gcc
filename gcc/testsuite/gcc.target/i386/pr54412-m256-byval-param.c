/* PR target/54412 */
/* Check that "assign_parm_setup_reg" does not emit an aligned store to an
   under-aligned local copy of an indirectly passed "__m256" parameter.

   MSYS2/MINGW-packages#1209: X needs 32-byte alignment, but under SEH the
   frame is only 16-byte aligned.  GCC put its home slot in a plain 16-byte
   slot, still treated it as 32-byte aligned and stored X with vmovaps.
   Nothing else sees X, so the move only has to match the slot.  Check the
   move instead of running: the bad slot is 32-byte aligned about half the
   time.  */
/* { dg-do compile { target x86_64-*-mingw* } } */
/* { dg-options "-O0 -mavx" } */
/* { dg-final { scan-assembler-not {vmovaps\t%ymm0, -[0-9]+\(%rbp\)} } } */

typedef float __m256 __attribute__ ((__vector_size__ (32),
				     __may_alias__));

void
foo (__m256 x)
{
  (void) x;
}
