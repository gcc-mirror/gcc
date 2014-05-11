unsigned long long
foo (int a)
{
  return a * 7 & 1ULL << 63;
}
