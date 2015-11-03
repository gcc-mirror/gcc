/* { dg-do compile } */
/* { dg-options "-Winline -O2 -fgnu89-inline" } */

inline int q(void);		 /* { dg-warning "body not available" "" } */
inline int t(void)
{
	return q();		 /* { dg-message "called from here" } */
}
