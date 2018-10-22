// PR c++/78948
// { dg-do compile { target c++17 } }

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
