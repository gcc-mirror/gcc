typedef int *__restrict__ pRINT;
typedef unsigned int *__restrict__ pRUINT;
typedef long long *__restrict__ pRINT64;
typedef unsigned long long *__restrict__ pRUINT64;
extern int abs (int j);

void test_orn (pRUINT a, pRUINT b, pRUINT c)
{
  int i;
  for (i = 0; i < 16; i++)
     c[i] = a[i] | (~b[i]);
}

void test_bic (pRUINT a, pRUINT b, pRUINT c)
{
  int i;
  for (i = 0; i < 16; i++)
     c[i] = a[i] & (~b[i]);
}

void mla (pRINT a, pRINT b, pRINT c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] += a[i] * b[i]; 
}

void mls (pRINT a, pRINT b, pRINT c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] -= a[i] * b[i];
}

void smax (pRINT a, pRINT b, pRINT c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] = (a[i] > b[i] ? a[i] : b[i]);
}

void smin (pRINT a, pRINT b, pRINT c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] = (a[i] < b[i] ? a[i] : b[i]);
}

void umax (pRUINT a, pRUINT b, pRUINT c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] = (a[i] > b[i] ? a[i] : b[i]);
}

void umin (pRUINT a, pRUINT b, pRUINT c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] = (a[i] < b[i] ? a[i] : b[i]);
}

unsigned int reduce_umax (pRUINT a)
{
  int i;
  unsigned int s = a[0];
  for (i = 1; i < 16; i++)
    s = (s > a[i] ? s : a[i]);

  return s;
}

unsigned int reduce_umin (pRUINT a)
{
  int i;
  unsigned int s = a[0];
  for (i = 1; i < 16; i++)
    s = (s < a[i] ? s : a[i]);

  return s;
}

int reduce_smax (pRINT a)
{
  int i;
  int s = a[0];
  for (i = 1; i < 16; i++)
    s = (s > a[i] ? s : a[i]);

  return s;
}

int reduce_smin (pRINT a)
{
  int i;
  int s = a[0];
  for (i = 1; i < 16; i++)
    s = (s < a[i] ? s : a[i]);

  return s;
}

unsigned int reduce_add_u32 (pRINT a)
{
  int i;
  unsigned int s = 0;
  for (i = 0; i < 16; i++)
    s += a[i];

  return s;
}

int reduce_add_s32 (pRINT a)
{
  int i;
  int s = 0;
  for (i = 0; i < 16; i++)
    s += a[i];

  return s;
}

unsigned long long reduce_add_u64 (pRUINT64 a)
{
  int i;
  unsigned long long s = 0;
  for (i = 0; i < 16; i++)
    s += a[i];

  return s;
}

long long reduce_add_s64 (pRINT64 a)
{
  int i;
  long long s = 0;
  for (i = 0; i < 16; i++)
    s += a[i];

  return s;
}

void sabd (pRINT a, pRINT b, pRINT c)
{
  int i;
  for (i = 0; i < 16; i++)
    c[i] = abs (a[i] - b[i]);
}

void saba (pRINT a, pRINT b, pRINT c)
{
  int i;
  for (i = 0; i < 16; i++)
    c[i] += abs (a[i] - b[i]);
}
