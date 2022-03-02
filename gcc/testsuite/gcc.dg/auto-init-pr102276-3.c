/* { dg-do compile } */
/* { dg-options "-O2 -Wtrivial-auto-var-init -ftrivial-auto-var-init=zero" } */

int g(int *, int *);
int f()
{
    switch (0) {
        int x;  /* { dg-warning "cannot be initialized with" } */
        int y;  /* { dg-warning "cannot be initialized with" } */
        default:
        return g(&x, &y);
    }
}

int g1(int, int);
int f1()
{
    switch (0) {
        int x; /* { dg-warning "cannot be initialized with" } */
        int y; /* { dg-warning "cannot be initialized with" } */
        default:
        return g1(x, y);
    }
}

struct S
{
  char a;
  int b;
};
int g2(int);
int f2(int input)
{
    switch (0) {
        struct S x; /* { dg-warning "cannot be initialized with" } */
        struct S y; /* { dg-warning "cannot be initialized with" } */
        default:
        return g2(input) + x.b + y.b;
    }
}
