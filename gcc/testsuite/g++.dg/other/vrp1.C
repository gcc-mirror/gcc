// { dg-do compile }
// { dg-options "-O2" }
// This caused a crash in VRP because TREE_OVERFLOW was set for MIN.

template<long long MIN>
long long mod (long long l, long long r)
{
  if (l == MIN && r == -1)
    return 0LL;
  return l % r;
}
template long long mod<-0x8000000000000000LL> (long long, long long);
