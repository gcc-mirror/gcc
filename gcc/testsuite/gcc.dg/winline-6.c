/* { dg-do compile } */
/* { dg-options "-Winline -O2 --param large-function-growth=0 --param large-function-insns=1 -fgnu89-inline" } */

void big (void);
inline int q(void) /* { dg-warning "large-function-growth" } */
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
	return q () + 1;	 /* { dg-message "called from here" } */
}
