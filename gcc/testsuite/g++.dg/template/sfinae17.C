// The conversion from D* to B* is ambiguous, but that should not produce
// an error, it should remove the first f overload by SFINAE.

#define static_assert(TEST,STR) \
  do { int ar[(TEST)?1:-1]; } while (0);

struct B {};

struct B1 : B {};
struct B2 : B {};

struct D : B1, B2 {};

template <class T> T create();

typedef char one[1];
typedef char two[2];

template <class T>
    one &f(char (*)[sizeof static_cast<T>(create<D *>())]);
template <class T>
    two &f(...);

int main()
{
  static_assert(sizeof f<int>(0) == sizeof(two), "");
  static_assert(sizeof f<B *>(0) == sizeof(two), "");
}
