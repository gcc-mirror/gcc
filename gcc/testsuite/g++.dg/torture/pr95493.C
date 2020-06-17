// { dg-do run }
// { dg-additional-options "-std=c++17 -Wno-psabi -w" }

struct verify
{
  const bool m_failed = false;

  [[gnu::noinline]] void print_hex(const void* x, int n) const
  {
    const auto* bytes = static_cast<const unsigned char*>(x);
    for (int i = 0; i < n; ++i)
      __builtin_printf((i && i % 4 == 0) ? "'%02x" : "%02x", bytes[i]);
    __builtin_printf("\n");
  }

  template <typename... Ts>
  verify(bool ok, const Ts&... extra_info) : m_failed(!ok)
  {
    if (m_failed)
      (print_hex(&extra_info, sizeof(extra_info)), ...);
  }

  ~verify()
  {
    if (m_failed)
      __builtin_abort();
  }
};

using K [[gnu::vector_size(16)]] = int;

int
main()
{
  int count = 1;
  asm("" : "+m"(count));
  verify(count == 1, 0, "", 0);

  {
    struct SW
    {
      K d;
    };
    struct
    {
      SW d;
    } xx;
    SW& x = xx.d;
    x = SW(); // [0, 0, 0, 0]
    for (int i = 3; i >= 2; --i)
      {
        x.d[i] = -1; // [0, 0, 0, -1] ...
        int a = [](K y) {
          for (int j = 0; j < 4; ++j)
            if (y[j] != 0)
              return j;
          return -1;
        }(x.d);
        verify(a == i, 0, 0, 0, 0, i, x);
      }
  }
}
