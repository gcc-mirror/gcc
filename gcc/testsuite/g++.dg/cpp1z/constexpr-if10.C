// PR c++/78948
// { dg-options -std=c++1z }

template <int T>
void sizeof_mismatch()
{
    static_assert(T == 0, "sizeof mismatch");
}

int main()
{
  if constexpr(sizeof(int*) == sizeof(char*))
    ;
  else
    sizeof_mismatch<sizeof(int*)>();
}
