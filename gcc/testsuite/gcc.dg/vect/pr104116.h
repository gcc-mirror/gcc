#define TEST_FN(OP, CONST, NAME) \
__attribute__((noinline)) \
void __GIMPLE (ssa,guessed_local(10737416)) \
NAME (int * a) \
{ \
  int i; \
  long unsigned int _1; \
  long unsigned int _2; \
  int * _3; \
  int _4; \
  int _5; \
  unsigned int _12; \
  unsigned int _13; \
 \
  __BB(2,guessed_local(10737416)): \
  goto __BB3(precise(134217728)); \
 \
  __BB(3,loop_header(1),guessed_local(1063004408)): \
  i_14 = __PHI (__BB5: i_11, __BB2: 0); \
  _13 = __PHI (__BB5: _12, __BB2: 1024u); \
  _1 = (long unsigned int) i_14; \
  _2 = _1 * 4ul; \
  _3 = a_9(D) + _2; \
  _4 = __MEM <int> (_3); \
  _5 = _4 OP CONST; \
  __MEM <int> (_3) = _5; \
  i_11 = i_14 + 1; \
  _12 = _13 - 1u; \
  if (_12 != 0u) \
    goto __BB5(guessed(132861994)); \
  else \
    goto __BB4(guessed(1355734)); \
 \
  __BB(5,guessed_local(1052266995)): \
  goto __BB3(precise(134217728)); \
 \
  __BB(4,guessed_local(10737416)): \
  return; \
 \
} \

#define TEST_FN_UNSIGNED(OP, CONST, NAME) \
__attribute__((noinline)) \
void __GIMPLE (ssa,guessed_local(10737416)) \
NAME (unsigned int * a) \
{ \
  int i; \
  long unsigned int _1; \
  long unsigned int _2; \
  unsigned int * _3; \
  unsigned int _4; \
  unsigned int _5; \
  unsigned int _12; \
  unsigned int _13; \
 \
  __BB(2,guessed_local(10737416)): \
  goto __BB3(precise(134217728)); \
 \
  __BB(3,loop_header(1),guessed_local(1063004408)): \
  i_14 = __PHI (__BB5: i_11, __BB2: 0); \
  _13 = __PHI (__BB5: _12, __BB2: 1024u); \
  _1 = (long unsigned int) i_14; \
  _2 = _1 * 4ul; \
  _3 = a_9(D) + _2; \
  _4 = __MEM <unsigned int> (_3); \
  _5 = _4 OP CONST; \
  __MEM <unsigned int> (_3) = _5; \
  i_11 = i_14 + 1; \
  _12 = _13 - 1u; \
  if (_12 != 0u) \
    goto __BB5(guessed(132861994)); \
  else \
    goto __BB4(guessed(1355734)); \
 \
  __BB(5,guessed_local(1052266995)): \
  goto __BB3(precise(134217728)); \
 \
  __BB(4,guessed_local(10737416)): \
  return; \
} \


#define N 1024
int arr[N];
void init_arr (int *a, int n)
{
  #pragma GCC novector
  for (int i=0; i<n; i++)
    a[i] = i - n/2;
}

unsigned int uarr[N];
void init_uarr (unsigned int *a, int n)
{
  #pragma GCC novector
  for (unsigned int i=0; i<n; i++)
    a[i] = 0xf0000000 + i;
}

int cl_div (int x, int y)
{
  int r = x % y;
  int q = x / y;
  if (r != 0 && (x ^ y) >= 0)
    q++;
  return q;
}

unsigned int cl_udiv (unsigned int x, unsigned int y)
{
  unsigned int r = x % y;
  unsigned int q = x / y;
  if (r > 0)
      q++;
  return q;
}

int cl_mod (int x, int y)
{
  int r = x % y;
  if (r != 0 && (x ^ y) >= 0)
    r -= y;
  return r;
}

unsigned int cl_umod (unsigned int x, unsigned int y)
{
  unsigned int r = x % y;
  unsigned int q = x / y;
  if (r > 0)
      r-=y;
  return r;
}

int fl_div (int x, int y)
{
  int r = x % y;
  int q = x / y;
  if (r != 0 && (x ^ y) < 0)
    q--;
  return q;
}

int fl_mod (int x, int y)
{
  int r = x % y;
  if (r != 0 && (x ^ y) < 0)
    r += y;
  return r;
}

int abs(int x)
{
  if (x < 0) return -x;
  return x;
}

int rd_mod (int x, int y)
{
  int r = x % y;
  if (abs(r) > abs((y-1) >> 1))
  {
    if ((x ^ y) < 0)
      r += y;
    else
      r -= y;
  }
  return r;
}

int rd_div (int x, int y)
{
  int r = x % y;
  int q = x / y;
  if (abs(r) > abs((y-1) >> 1))
  {
    if ((x ^ y) < 0)
      q--;
    else
      q++;
  }
  return q;
}

unsigned int rd_umod (unsigned int x, unsigned int y)
{
  unsigned int r = x % y;
  if (r > ((y-1) >> 1))
      r -= y;
  return r;
}

unsigned int rd_udiv (unsigned int x, unsigned int y)
{
  unsigned int r = x % y;
  unsigned int q = x / y;
  if (r > ((y-1) >> 1))
      q++;
  return q;
}
