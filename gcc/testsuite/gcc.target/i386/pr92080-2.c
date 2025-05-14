/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3" } */
/* { dg-final { scan-assembler-times "vpxor" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpeq" 1 } } */

typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef short v8hi __attribute__((vector_size(16)));
typedef short v16hi __attribute__((vector_size(32)));
typedef char v16qi __attribute__((vector_size(16)));
typedef char v32qi __attribute__((vector_size(32)));

v16qi b1;
v8hi h1;
v4si s1;
v32qi b2;
v16hi h2;
v8si s2;

void
foo (int i, int j)
{
  switch (i)
    {
    case 1:
      h1 = __extension__(v8hi){-1, -1, -1, -1, -1, -1, -1, -1};
      s1 = __extension__(v4si){0, 0, 0, 0};
      s2 = __extension__(v8si){0, 0, 0, 0, 0, 0, 0, 0};
      break;
    case 2:
      h1 = __extension__(v8hi){0, 0, 0, 0, 0, 0, 0, 0};
      b1 = __extension__(v16qi){-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
      break;
    case 3:
      h1 = __extension__(v8hi){0, 0, 0, 0, 0, 0, 0, 0};
      b1 = __extension__(v16qi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
      break;
    default:
      break;
    }

  switch (i)
    {
    case 1:
      s1 = __extension__(v4si){-1, -1, -1, -1};
      b2 = __extension__(v32qi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
      h2 = __extension__(v16hi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
      break;
    case 2:
      b1 = __extension__(v16qi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
      h1 = __extension__(v8hi){-1, -1, -1, -1, -1, -1, -1, -1};
      break;
    case 3:
      b1 = __extension__(v16qi){-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
      s2 = __extension__(v8si){-1, -1, -1, -1, -1, -1, -1, -1};
      break;
    }
}
