typedef int __attribute__((vector_size(8))) v2si;
long long s64;

static inline long long
__ev_convert_s64 (v2si a)
{
  return (long long) a;
}

int main()
{
  s64 = __ev_convert_s64 ((v2si){1,0xffffffff});
  if (s64 != 0x1ffffffffLL)
    abort ();
  return 0;
}
