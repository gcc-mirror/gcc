/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fpic -fprofile -mfentry -O2 -mcmodel=large" } */

void
func (void)
{
} /* { dg-message "sorry, unimplemented: profiling '-mcmodel=large' with PIC is not supported" } */
