/* { dg-do compile } */
/* { dg-options "-Winline -O2 --param inline-unit-growth=0 --param large-unit-insns=0" } */

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
inline int q1(void)
{
	big();
	big();
	big();
}
int t (void)
{
 /* We allow one inlining over limit.  */
	q1();
	return q ();		 /* { dg-warning "called from here" } */
}
