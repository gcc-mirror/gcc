/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-minline-all-stringops" { target { i?86-*-* x86_64-*-* } } } */

extern void abort (void);

#define MAX_OFFSET (sizeof (long long))
#define MAX_COPY (8 * sizeof (long long))
#define MAX_EXTRA (sizeof (long long))

#define MAX_LENGTH (MAX_OFFSET + MAX_COPY + MAX_EXTRA)

static union {
  char buf[MAX_LENGTH];
  long long align_int;
  long double align_fp;
} u;

char A[MAX_LENGTH];

int
main ()
{
  int off, len, i;
  char *p, *q;

  for (i = 0; i < MAX_LENGTH; i++)
    A[i] = 'A';

  for (off = 0; off < MAX_OFFSET; off++)
    for (len = 1; len < MAX_COPY; len++)
      {
	for (i = 0; i < MAX_LENGTH; i++)
	  u.buf[i] = 'a';

	p = __builtin_memcpy (u.buf + off, A, len);
	if (p != u.buf + off)
	  abort ();

	q = u.buf;
	for (i = 0; i < off; i++, q++)
	  if (*q != 'a')
	    abort ();

	for (i = 0; i < len; i++, q++)
	  if (*q != 'A')
	    abort ();

	for (i = 0; i < MAX_EXTRA; i++, q++)
	  if (*q != 'a')
	    abort ();
      }

  return 0;
}
