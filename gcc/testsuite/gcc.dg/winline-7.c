/* { dg-do compile } */
/* { dg-options "-Winline -O2" } */

void big (void);
inline int q(void)
{				/* { dg-warning "(function not inlinable|alloca)" } */
	return (int)alloca(10);
}
inline int t (void)
{
	return q ();		 /* { dg-warning "called from here" } */
}
