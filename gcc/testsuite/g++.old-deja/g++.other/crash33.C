// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

template <class T>
inline const T& bar(const T& a, const T& b)
{
  return a < b ? b : a;
}

int foo(void)
{
  return bar(sizeof(int), sizeof(long));
}
