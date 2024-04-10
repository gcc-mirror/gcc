// { dg-do run }
// { dg-additional-options "-ffast-math" }

double a1 = 1.0;
double a2 = 1.0;

void __attribute__((noipa))
f(double K[2], bool b)
{
    double A[] = {
        b ? a1 : a2,
        0,
        0,
        0
    };

    double sum{};
    for(double  a : A) sum += a;
    for(double& a : A) a /= sum;

    if (b) {
        K[0] = A[0]; // 1.0
        K[1] = A[1]; // 0.0
    } else {
        K[0] = A[0] + A[1];
    }
}

int main()
{
  double K[2]{};
  f(K, true);
  if (K[0] != 1. || K[1] != 0.)
    __builtin_abort ();
}
