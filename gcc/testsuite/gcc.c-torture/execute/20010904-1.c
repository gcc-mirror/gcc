typedef struct x { int a; int b; } __attribute__((aligned(32))) X;
typedef struct y { X x[32]; int c; } Y;

Y y[2];

int main(void)
{
  if (((char *)&y[1] - (char *)&y[0]) & 31)
    abort ();
  exit (0);
}                
