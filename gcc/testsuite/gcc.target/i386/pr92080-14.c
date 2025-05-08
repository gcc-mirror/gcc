/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastd" 1 } } */

typedef int v16si __attribute__((vector_size(64)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));

extern v16si sinksz;
extern v8si sinksy;
extern v4si sinksx;
extern v4si sinksx1;

extern void bar (void);

void
foo (char c, int i)
{
  sinksz = __extension__(v16si){c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c};
  if (i == 1)
    {
      sinksy = __extension__(v8si){c,c,c,c,c,c,c,c};
      bar ();
    }
  else if (i == 2)
    {
      sinksx = __extension__(v4si){c,c,c,c};
      bar ();
    }
  sinksx1 = __extension__(v4si){c,c,c,c};
}
