/* { dg-do compile } */
/* { dg-options "-O2 -Wall -ftrivial-auto-var-init=pattern" } */

int g(int *);
int f()
{
    switch (0) { 
        int x;  /* { dg-bogus "statement will never be executed" } */
        default:
        return g(&x);
    }
}

int g1(int);
int f1()
{
    switch (0) {
        int x; /* { dg-bogus "statement will never be executed" } */
        default:
        return g1(x);  /* { dg-warning "is used uninitialized" } */
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
        struct S x; /* { dg-bogus "statement will never be executed" } */
        default:
        return g2(input) + x.b;  /* { dg-warning "is used uninitialized" } */
    }
}

