/* { dg-do compile } */
/* { dg-options "-Winline -O1 -fno-unit-at-a-time" } */

inline int q(void);		 /* { dg-warning "body not available" } */
inline int t(void)
{
	return q();		 /* { dg-warning "called from here" } */
}
int q(void)
{
}
