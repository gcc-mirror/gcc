typedef int I;

int f(float I = 0.0, int b = I(2)); // { dg-error "parameter" }
int g(int b = I(2), float I = 0.0);

struct A
{
  int f(float I = 0.0, int b = I(2)); // { dg-error "parameter" }
  int g(int b = I(2), float I = 0.0);
};

