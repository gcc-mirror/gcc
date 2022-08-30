/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

typedef struct
{
 int a;
 int b;
 int c;
 int d;
 int e;
 int f;
 int g;
 int h;
 int i;
 int j;
} X;

X *XX(int);

int G();

static void F()
{
 X *x;
 int m, n;
 int xa, xb, xc, xd, xe, xf, xg, xh, xi, xj;

 m = G();
 n = G();
 if ( n & 1 ) xa = G();
 if ( n & 2 ) xb = G();
 if ( n & 4 ) xc = G();
 if ( n & 32 ) xd = G();
 if ( n & 16 ) xe = G();
 if ( n & 64 ) xf = G();
 if ( n & 256 ) xg = G();
 if ( n & 512 ) xh = G();
 if ( n & 1024 ) xi = G();
 if ( n & 2048 ) xj = G();

 if ( m >= 64 ) return;
 x = XX(m);
 if ( n & 1 ) x->a = xa;
 if ( n & 2 ) x->b = xb;
 if ( n & 4 ) x->c = xc;
 if ( n & 32 ) x->d = xd;
 if ( n & 16 ) x->e = xe;
 if ( n & 64 ) x->f = xf;
 if ( n & 256 ) x->g = xg;
 if ( n & 512 ) x->h = xh;
 if ( n & 1024 ) x->i = xi;
 if ( n & 2048 ) x->j = xj; /* { dg-bogus "uninitialized" } */
}

void H()
{
 F();
}
