// { dg-do compile }
// { dg-options "-g" }

template <typename T>
struct QCachedT
{
  void operator delete(void *, T *) {}
};
template<int a>
void exercise()
{
  struct thing_t
    : QCachedT<thing_t>
  {
  };
  thing_t *list[1];
  new thing_t; // { dg-warning "" }
}
int main() { exercise<1>(); }
