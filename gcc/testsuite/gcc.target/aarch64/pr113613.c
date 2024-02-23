// { dg-options "-O2" }

typedef float __attribute__((vector_size(8))) v2sf;
v2sf a[4];
v2sf b[4];
void f()
{
  b[0] += a[0];
  b[1] += a[1];
}

// { dg-final { scan-assembler-times {\tldp\t} 2 } }
// { dg-final { scan-assembler-times {\tstp\t} 1 } }
