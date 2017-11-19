/* { dg-do compile } */
/* { dg-options "-O -fno-rounding-math -fno-signed-zeros -fdump-tree-optimized-raw" } */

double f(double a, double b){
    double c = a - b;
    return -c;
}

int g(unsigned x){
    unsigned y = ~x;
    int z = (int) y;
    return -z;
}

unsigned h(unsigned a, unsigned b, unsigned c){
    unsigned d = b - c;
    unsigned e = a + d;
    return -e;
}

/* { dg-final { scan-tree-dump-not "negate_expr" "optimized"} } */
