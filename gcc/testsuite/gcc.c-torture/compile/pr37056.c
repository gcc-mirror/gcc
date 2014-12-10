/* { dg-skip-if "ptxas times out" { nvptx-*-* } { "-O2" "-Os" } { "" } } */
extern void abort (void);

static union {
    char buf[12 * sizeof (long long)];
} u;

int main ()
{
  int off, len, i;
  char *p, *q;

  for (off = 0; off < (sizeof (long long)); off++)
    for (len = 1; len < (10 * sizeof (long long)); len++)
      {
	for (i = 0; i < (12 * sizeof (long long)); i++)
	  u.buf[i] = 'a';
	p = (__extension__ (__builtin_constant_p ('\0') && ('\0') == '\0'
			    ? ({void *__s = (u.buf + off); __s;})
			    : __builtin_memset (u.buf + off, '\0', len)));
	if (p != u.buf + off)
	  abort ();
	for (i = 0; i < off; i++, q++)
	  if (*q != 'a')
	    abort ();
      }
  return 0;
}

