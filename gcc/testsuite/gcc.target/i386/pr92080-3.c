/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */
/* { dg-final { scan-assembler-times "pxor" 1 } } */
/* { dg-final { scan-assembler-times "pcmpeq" 1 } } */

typedef int v4si __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
typedef char v16qi __attribute__((vector_size(16)));

v16qi b1;
v8hi h1;
v4si s1;

void
foo (int i, int j)
{
  switch (i)
    {
    case 1:
      h1 = __extension__(v8hi){-1, -1, -1, -1, -1, -1, -1, -1};
      s1 = __extension__(v4si){0, 0, 0, 0};
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
      break;
    case 2:
      b1 = __extension__(v16qi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
      h1 = __extension__(v8hi){-1, -1, -1, -1, -1, -1, -1, -1};
      break;
    case 3:
      b1 = __extension__(v16qi){-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
      break;
    }
}
