#define NMAX 2000

static double x1[NMAX], x2[NMAX], a[NMAX][NMAX], y_1[NMAX], y_2[NMAX];

void mvt(long N) {

    int i,j;

    for (i=0; i<N; i++) {
        for (j=0; j<N; j++) {
            x1[i] = x1[i] + a[i][j] * y_1[j];
        }
    }
    
    for (i=0; i<N; i++) {
        for (j=0; j<N; j++) {
            x2[i] = x2[i] + a[j][i] * y_2[j];
        }
    }
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" { xfail *-*-* } } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */

