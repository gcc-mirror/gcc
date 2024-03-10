#define NOINLINE __attribute__ ((noinline))
UINT NOINLINE
opt_u1 (UINT x)
{
  if (x < (M * N) - GAP)
    return 0;
  UINT a = x - (M * N);
  UINT b = a / N;
  return b + M;
}

UINT NOINLINE
opt_u2 (UINT x)
{
  if (x > (UMAX - (M * N) + GAP))
    return 0;
  UINT a = x + (M * N);
  UINT b = a / N;
  return b - M;
}

INT NOINLINE
opt_s1 (INT x)
{
  if (x < (M * N) - GAP)
    return 0;
  INT a = x - (M * N);
  INT b = a / N;
  return b + M;
}

INT NOINLINE
opt_s2 (INT x)
{
  if (x < IMIN + (M * N) - GAP || x > 0)
    return 0;
  INT a = x - (M * N);
  INT b = a / N;
  return b + M;
}

INT NOINLINE
opt_s3 (INT x)
{
  if (x < (M * N) - GAP)
    return 0;
  INT a = x - (M * N);
  INT b = a / -N;
  return b + -M;
}

INT NOINLINE
opt_s4 (INT x)
{
  if (x < IMIN + (M * N) - GAP || x > 0)
    return 0;
  INT a = x - (M * N);
  INT b = a / -N;
  return b + -M;
}

INT NOINLINE
opt_s5 (INT x)
{
  if (x > (-M * N) + GAP)
    return 0;
  INT a = x - (-M * N);
  INT b = a / N;
  return b + -M;
}

INT NOINLINE
opt_s6 (INT x)
{
  if (x > IMAX - (M * N) + GAP || x < 0)
    return 0;
  INT a = x - (-M * N);
  INT b = a / N;
  return b + -M;
}

INT NOINLINE
opt_s7 (INT x)
{
  if (x > (M * -N) + GAP)
    return 0;
  INT a = x - (M * -N);
  INT b = a / -N;
  return b + M;
}

INT NOINLINE
opt_s8 (INT x)
{
  if (x > IMAX - (M * N) + GAP || x < 0)
    return 0;
  INT a = x - (M * -N);
  INT b = a / -N;
  return b + M;
}

UINT NOINLINE
opt_u3 (UINT x)
{
  if (x < (M << N) - GAP)
    return 0;
  UINT a = x - (M << N);
  UINT b = a >> N;
  return b + M;
}

UINT NOINLINE
opt_u4 (UINT x)
{
  if (x > (UMAX - (M << N)) + GAP)
    return 0;
  UINT a = x + (M << N);
  UINT b = a >> N;
  return b - M;
}

INT NOINLINE
opt_s9 (INT x)
{
  if (x < (M << N) - GAP)
    return 0;
  INT a = x - (M << N);
  INT b = a >> N;
  return b + M;
}

INT NOINLINE
opt_s10 (INT x)
{
  if (x < IMIN + (M << N) - GAP || x > 0)
    return 0;
  INT a = x - (M << N);
  INT b = a >> N;
  return b + M;
}

INT NOINLINE
opt_s11 (INT x)
{
  if (x > (-M << N) + GAP)
    return 0;
  INT a = x - (-M << N);
  INT b = a >> N;
  return b + -M;
}

INT NOINLINE
opt_s12 (INT x)
{
  if (x > IMAX - (M << N) + GAP || x < 0)
    return 0;
  INT a = x - (-M << N);
  INT b = a >> N;
  return b + -M;
}

UINT NOINLINE
opt_u5 (UINT x, UINT n, UINT m)
{
  if (n > N || m > M)
    return 0;
  if (x < (M*N) - GAP)
    return 0;
  UINT a = x - (m * n);
  UINT b = a / n;
  return b + m;
}

UINT NOINLINE
opt_u6 (UINT x, UINT n, UINT m)
{
  if (n > N || m > M)
    return 0;
  if (x > (UMAX - M*N) + GAP)
    return 0;
  UINT a = x + (m * n);
  UINT b = a / n;
  return b - m;
}

INT NOINLINE
opt_s13 (INT x, INT n, INT m)
{
  if (n > N || m > M || n < 0 || m < 0)
    return 0;
  if (x < (M*N) - GAP)
    return 0;
  INT a = x - (m * n);
  INT b = a / n;
  return b + m;
}

INT NOINLINE
opt_s14 (INT x, INT n, INT m)
{
  if (n > N || m > M || n < 0 || m < 0)
    return 0;
  if (x > -M*N + GAP)
    return 0;
  INT a = x + (m * n);
  INT b = a / n;
  return b - m;
}

INT
opt_s15 (INT x, INT n, INT m)
{
  if (n > 0 || m > 0 || n < -N || m < -M)
    return 0;
  if (x < (M*N) - GAP)
    return 0;
  INT a = x - (m * n);
  INT b = a / n;
  return b + m;
}

INT NOINLINE
opt_s16 (INT x, INT n, INT m)
{
  if (n > 0 || m > 0 || n < -N || m < -M)
    return 0;
  if (x < 0 || x > (IMAX - M*N) + GAP)
    return 0;
  INT a = x + (m * n);
  INT b = a / n;
  return b - m;
}

