/* { dg-do compile } */
/* { dg-options "-Winline -O2 --param max-inline-insns-single=1 --param inline-min-speedup=100 -fgnu89-inline" } */

void big (void);
inline int q(void) /* { dg-warning "max-inline-insns-single" } */
{
	big();
	big();
	big();
	big();
	big();
	big();
	big();
	big();
	big();
	big();
}
inline int t (void)
{
	return q ();		 /* { dg-message "called from here" } */
}
