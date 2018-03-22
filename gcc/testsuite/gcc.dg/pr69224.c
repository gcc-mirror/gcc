/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds" } */

struct S {
        int a;
        int b;
        int c;
        int d;
        int e;
        float x[5];
        float y[5];      // Comment these two lines out to
        float z[5 - 1];  // remove the warning
};
void f(struct S *s, float a[], float **b, float c[]) {
        if ((s->b == 1) && (s->d > 0)) {
                for (int i = 0; i < s->a; i++) {
                        if (a[i] != 0.0) {
                                for (int j = 0; j < s->d - 1; j++) {
                                        if ((c[i] >= s->x[j]) && (c[i] <= s->x[j + 1])) {
                                                b[2*j][i] = s->x[j];
                                                break;
                                        }
                                }
                        }
                }
        }
}
