/* PR debug/113918 */
/* { dg-do compile } */
/* { dg-options "-gdwarf-5 -dA -fno-merge-debug-strings" } */

struct S {
  union {
    int i;
    long long j;
  };
  struct {
    int k;
    long long l;
  };
  union {
    int m;
    long long n;
  } u;
  struct {
    int o;
    long long p;
  } v;
} s;

int
main ()
{
  s.i = 1;
  s.k = 2;
  s.u.m = 3;
  s.v.o = 4;
}

/* { dg-final { scan-assembler-times "DW_AT_export_symbols" 4 } } */
