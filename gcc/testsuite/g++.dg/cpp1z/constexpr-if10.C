// PR c++/79848
// { dg-options -std=c++1z }

template <int T>
void sizeof_mismatch()
{
    static_assert(T == 0, "sizeof mismatch");
}

int main()
{
  if constexpr(sizeof(long long) == sizeof(char*))
    ;
  else
    sizeof_mismatch<sizeof(long long)>();
}
