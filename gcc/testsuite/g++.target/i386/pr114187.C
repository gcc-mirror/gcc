/* { dg-do compile } */
/* { dg-options "-O2" } */

struct P2d {
    double x, y;
};

double sumxy_p(P2d p) {
    return p.x + p.y;
}

/* { dg-final { scan-assembler-not "movq" } } */
/* { dg-final { scan-assembler-not "xchg" } } */
