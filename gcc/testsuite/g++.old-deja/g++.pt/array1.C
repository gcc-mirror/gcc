template <class T, unsigned N>
  unsigned size(T (&)[N]) { return N; }
template <class T, unsigned N>
  unsigned size(T const (&)[N]) { return N; }

int main() {
  short iarray[] = { 1, 2, 3, 4, 5 };
  const short carray[] = { 1, 2, 3, 4, 5 };
  return size(iarray) - size(carray);
}
