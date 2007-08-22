/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-Os -fpic" } */

#include <string.h>

typedef struct
{
  char *a;
  char *b;
} *foo;

void
bar (foo x)
{
  char *c = x->b;
  char *d = (void *)0;
  unsigned int e = 0, f = 0, g;
  while (*c != ':')
    if (*c == '%')
      {
        ++c;
        switch (*c++)
          {
          case 'N':
            g = strlen (x->a);
            if (e + g >= f) {
		char *h = d;
		f += 256 + g;
		d = (char *) __builtin_alloca (f);
		memcpy (d, h, e);
	    };
            memcpy (&d[e], x->a, g);
            e += g;
            break;
	  }
      }
}
