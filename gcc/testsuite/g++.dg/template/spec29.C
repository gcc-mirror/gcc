// PR c++/25855
// { dg-do run }

template <typename T>  int qCompare(const T *t1, const T *t2) { return 1; }
template <typename T>  int qCompare(T *t1, T *t2) { return 2; }
template <typename T1, typename T2> int qCompare(const T1 *t1, const T2 *t2) {
  return 3; }
template<> int qCompare(const char *t1, const char *t2) { return 4; }
int main()
{
  if (qCompare("a", "b") != 4)
    return 1;
}
