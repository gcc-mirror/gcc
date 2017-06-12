/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-additional-options "-fprofile-generate" } */

extern int isspace (int);

int foo(const char *txt, char *buf)
{
  const char *s;
  char *d;
  int ws = 1;
  for (s=txt, d=buf; *s; )
    {
      if (*s=='/' && *(s+1)=='/') {

	  s += 2;
	  while (*s && *s!='\r' && *s!='\n')
	    s++;
      }
      else if (*s=='"') {

	  s++;
	  while (*s && *s!='\r' && *s!='\n' && *s!='"')
	    if (*s++=='\\')
	      s++;
	  if (*s=='"')
	    s++;
      }
      else {
	  if (*s && !isspace(*s))
	    ws = 0;


	  *d++ = *s++;

      }
    }
  *d = '\0';

  return ws;
}

/* { dg-final { scan-tree-dump "base object not addressable" "slp1" } } */
/* { dg-final { scan-tree-dump-not "MEM\[^\r\n\]*__gcov\[^\r\n\]* = vect_cst" "slp1" } } */
