/* { dg-additional-options "-std=gnu89" } */

typedef struct { double x, y; } p;
typedef struct { int s; float r; } t;
t *e, i;
int i1;

f(t *op)
{
int i2 = e->r;
p pt;
int c = g();
t p;

if (c)
{
i = *e;
e -= 3;
return 8;
}
if (op > e)
return 1;
op->r = pt.x;
op->r = pt.y;
p = *e;
++e;
e->r = i1, e->s = i1;
*++e = p;
return 3;
}
