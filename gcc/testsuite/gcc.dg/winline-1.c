/* { dg-do compile } */
/* { dg-options "-Winline -O2" } */

void q(void);
inline int t(void)
{
	int ret;
	q();
	ret = t();  /* We define sane semantics for inline keyword on recursive
		       functions, so do not warn here.  */
	q();
	return ret;
}
