/* PR tree-optimization/92860 */
/* { dg-do compile } */
/* { dg-options "-Winline -O2 -fgnu89-inline" } */

#pragma GCC push_options
#pragma GCC optimize("-O0")
#pragma GCC pop_options

inline int q(void);		 /* { dg-warning "body not available" } */
inline int t(void)
{
	return q();		 /* { dg-message "called from here" } */
}
