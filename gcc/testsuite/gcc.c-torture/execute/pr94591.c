typedef unsigned __attribute__((__vector_size__(8))) V2SI_u;
typedef int __attribute__((__vector_size__(8))) V2SI_d;

typedef unsigned long __attribute__((__vector_size__(16))) V2DI_u;
typedef long __attribute__((__vector_size__(16))) V2DI_d;

void id_V2SI(V2SI_d *v)
{
  *v = __builtin_shuffle(*v, (V2SI_d)(V2SI_u) { 0, 1 });
}

void id_V2DI(V2DI_d *v)
{
  *v = __builtin_shuffle(*v, (V2DI_d)(V2DI_u) { 0, 1 });
}

extern void abort(void);

int main(void)
{
  V2SI_d si = { 35, 42 };
  id_V2SI(&si);

  if (si[0] != 35 || si[1] != 42)
    abort();

  V2DI_d di = { 63, 38 };
  id_V2DI(&di);

  if (di[0] != 63 || di[1] != 38)
    abort();
}
