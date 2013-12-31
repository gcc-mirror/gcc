/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f" } */

long long *D;
int *S;
short *H;
char *Q;

long long foo_unpack_1 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    D[i] *= S[i];

  return D[ind];
}

long long foo_unpack_2 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    D[i] *= H[i];

  return D[ind];
}

long long foo_unpack_3 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    D[i] *= Q[i];

  return D[ind];
}

int foo_unpack_4 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    S[i] *= H[i];

  return S[ind];
}

int foo_unpack_5 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    S[i] *= Q[i];

  return S[ind];
}

short foo_unpack_6 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    H[i] *= Q[i];

  return H[ind];
}

int foo_expand_1 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    S[i] *= D[i];

  return S[ind];
}

short foo_expand_2 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    H[i] *= D[i];

  return H[ind];
}

char foo_expand_3 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    Q[i] *= D[i];

  return Q[ind];
}

short foo_expand_4 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    H[i] *= S[i];

  return H[ind];
}

char foo_expand_5 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    Q[i] *= S[i];

  return Q[ind];
}

char foo_expand_6 (int low, int high, int ind)
{
  int i;

  for (i = low; i <= high; i++)
    Q[i] *= H[i];

  return Q[ind];
}
