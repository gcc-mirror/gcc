typedef struct
{
  int a, b;
} T;

f (T *bs)
{
  long long x;
  x = ({
    union { T s; long long l; } u;
    u.s = *bs;
    u.l;
  });
}
