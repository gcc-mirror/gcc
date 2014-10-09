/* { dg-do compile } */

union U {
    double val;
    union U *ptr;
};

union U *d;
double a;
int b;
int c;

static void fn1(union U *p1, int p2, _Bool p3)
{
    union U *e;

    if (p2 == 0)
	a = ((union U*)((__SIZE_TYPE__)p1 & ~1))->val;

    if (b) {
	e = p1;
    } else if (c) {
	e = ((union U*)((__SIZE_TYPE__)p1 & ~1))->ptr;
	d = e;
    } else {
	e = 0;
	d = ((union U*)0)->ptr;
    }

    fn1 (e, 0, 0);
    fn1 (0, 0, p3);
}

void fn2 (void)
{
  fn1 (0, 0, 0);
}
