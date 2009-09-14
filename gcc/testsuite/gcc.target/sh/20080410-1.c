/* { dg-do compile { target "sh-*-*" } } */
/* { dg-options "-O0 -m4 -ml" } */
/* { dg-final { scan-assembler-not "add\tr0,r0" } } */

/* This test checks that chain reloads conflict.  I they don't
   conflict, the same hard register R0 is used for the both reloads
   but in this case the second reload needs an intermediate register
   (which is the reload register).  As the result we have the
   following code

   	mov	#4,r0   -- first reload
	mov	r14,r0  -- second reload
	add	r0,r0   -- second reload

   The right code should be

   	mov	#4,r0   -- first reload
	mov	r14,r1  -- second reload
	add	r0,r1   -- second reload

*/

_Complex float foo_float ();

void bar_float ()
{
  __real foo_float ();
}
