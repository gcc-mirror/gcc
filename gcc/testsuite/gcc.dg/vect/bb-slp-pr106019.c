/* { dg-do compile } */

void f(double *p, long i)
{
    p[i+0] += 1;
    p[i+1] += 1;
}
void g(double *p, long i)
{
    double *q = p + i;
    q[0] += 1;
    q[1] += 1;
}

/* { dg-final { scan-tree-dump-not "can't determine dependence" slp2 } } */
