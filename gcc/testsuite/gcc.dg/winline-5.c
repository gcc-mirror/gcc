/* { dg-do compile } */
/* { dg-options "-Winline -O2 --param inline-unit-growth=0 --param large-unit-insns=0 -fgnu89-inline" } */

void big (void);
inline int q(void) /* { dg-warning "inline-unit-growth" } */
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
int t (void)
{
	return q ();		 /* { dg-message "called from here" } */
}
