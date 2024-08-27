/* { dg-require-effective-target vect_condition } */
#include "tree-vect.h"

extern void abort (void);

int
f1 (char *s)
{
  int c = 0;
  int i;
  for (i = 0; i < 64; i++)
    {
      switch (*s)
	{
	case ',':
	case '|':
	  c++;
	}
      s++;
    }
  return c;
}

int
f2 (char *s)
{
  int c = 0;
  int i;
  for (i = 0; i < 64; i++)
    {
      if (*s != '#')
	{
	  switch (*s)
	    {
	    case ',':
	    case '|':
	      c++;
	    }
	}
      s++;
    }
  return c;
}

int
f3 (char *s)
{
  int c = 0;
  int i;
  for (i = 0; i < 64; i++)
    {
      if (*s != '#')
        if (*s == ',' || *s == '|' || *s == '@' || *s == '*')
	  c++;
      s++;
    }
  return c;
}


int
f4 (char *s)
{
  int c = 0;
  int i;
  for (i = 0; i < 64; i++)
    {
      if (*s == ',' || *s == '|' || *s == '@' || *s == '*')
	c++;
      s++;
    }
  return c;
}

#define CHECK(f, str, res) \
  __builtin_strcpy(buf, str); n = f(buf); if (n != res) abort();

int
main ()
{
  int n;
  char buf[64];

  __builtin_memset (buf, 0, sizeof buf);

  check_vect ();

  CHECK (f1, ",,,,,,,,,,", 10);
  CHECK (f1, "||||||||||", 10);
  CHECK (f1, "aaaaaaaaaa", 0);
  CHECK (f1, "", 0);
  CHECK (f1, ",|,|xxxxxx", 4);

  CHECK (f2, ",,,,,,,,,,", 10);
  CHECK (f2, "||||||||||", 10);
  CHECK (f2, "aaaaaaaaaa", 0);
  CHECK (f2, "", 0);
  CHECK (f2, ",|,|xxxxxx", 4);

  CHECK (f3, ",,,,,,,,,,", 10);
  CHECK (f3, "||||||||||", 10);
  CHECK (f3, "aaaaaaaaaa", 0);
  CHECK (f3, "", 0);
  CHECK (f3, ",|,|xxxxxx", 4);

  CHECK (f4, ",,,,,,,,,,", 10);
  CHECK (f4, "||||||||||", 10);
  CHECK (f4, "aaaaaaaaaa", 0);
  CHECK (f4, "", 0);
  CHECK (f4, ",|,|xxxxxx", 4);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect"  } } */
