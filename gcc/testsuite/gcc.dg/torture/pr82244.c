/* { dg-do compile } */

typedef struct a {
  struct a *b;
} a;

extern int d(void);
extern int g(void);
extern int h(void);
extern int _setjmp(void *);

int c(void)
{
  1 ? d() : 0;

  a *e;
  while (e) {
      e = (e == (a *) c) ? 0 : e->b;
      while (e) {
	  int f = 0;
	  g();
	  if (_setjmp(0)) {
	      if (f & 6) {
		  ;
	      } else if (f & 2) {
		  h();
	      }
	  }
      }
  }
}
