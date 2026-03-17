typedef long long v2i64 __attribute__ ((vector_size (16), aligned (16)));
v2i64 a, b;

void
test (int l)
{
  a = b >> (-l);
}
