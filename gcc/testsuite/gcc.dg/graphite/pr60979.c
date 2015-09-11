/* { dg-options "-O -fgraphite-identity" } */
/* { dg-require-effective-target nonlocal_goto } */

#include <setjmp.h>

struct x;

typedef struct x **(*a)(struct x *);
void d (const char *);

struct x {
    union {
	struct {
	    union {
		a *i;
	    } l;
	    int s;
	} y;
    } e;
};

jmp_buf c;

void
b(struct x *r)
{
  int f;
  static int w = 0;
  volatile jmp_buf m;
  f = (*(((struct x *)r)->e.y.l.i[2]((struct x *)r)))->e.y.s;
  if (w++ != 0)
    __builtin_memcpy((char *)m, (const char *)c, sizeof(jmp_buf));
  if (setjmp (c) == 0) {
      int z;
      for (z = 0; z < 0; ++z)
	;
  }
  d((const char *)m);
}
