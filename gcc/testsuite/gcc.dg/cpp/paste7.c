/* { dg-do run } */

#define D_2 1, 2
#define C_2(X, I0, I1) X##_a = I0, X##_b = I1
#define B_2(X, I) C_2(X, I)
#define A(N, X) B_##N (X, D_##N)

extern void abort(void);
extern void exit(int);

int x_a, x_b;

int main(void)
{
  A(2, x);
  if (x_a != 1 || x_b != 2)
    abort();
  exit(0);
}
