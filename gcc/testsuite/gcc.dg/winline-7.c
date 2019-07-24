/* { dg-do compile } */
/* { dg-options "-Winline -O2 -fgnu89-inline" } */
/* { dg-require-effective-target alloca } */

extern void *alloca (__SIZE_TYPE__);

void big (void);
inline void *q (void) /* { dg-warning "(function not inlinable|alloca)" } */
{
	return alloca (10);
}
inline void *t (void)
{
	return q ();		 /* { dg-message "called from here" } */
}

/* { dg-prune-output "-Wreturn-local-addr" } */
