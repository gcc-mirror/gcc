/* { dg-do compile } */
/* { dg-options "-std=c23" } */

void b(void)
{
	int n = 3;

	  extern struct f { char (*x)[3]; char (*y)[]; } q;
	{ extern struct f { char (*x)[]; char (*y)[4]; } q; 
	  _Static_assert(3 == sizeof(*q.x), "");
	  _Static_assert(4 == sizeof(*q.y), "");
	}
	{ extern struct f { char (*x)[2]; char (*y)[]; } q; (void)q; }	/* { dg-error "conflicting" } */

	{ struct f { char (*x)[n]; char (*y)[3]; }* qp = &q; (void)*qp; }
	(void)q;

	  static struct g { int a; char buf[n]; } *p; (void)p;
	{ static struct g { int a; char buf[3]; } *p; (void)p; }

	  static struct h { int a; void (*s)(char buf[n]); } *t; (void)t;
	{ static struct h { int a; void (*s)(char buf[3]); } *t; (void)t; }
}



