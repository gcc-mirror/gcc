/* { dg-do compile } */
/* { dg-options "-Winline -O2 --param max-inline-insns-single=1" } */

void big (void);
inline int q(void)		
{				/* { dg-warning "max-inline-insns-single" "" } */
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
	return q ();		 /* { dg-warning "called from here" "" } */
}
