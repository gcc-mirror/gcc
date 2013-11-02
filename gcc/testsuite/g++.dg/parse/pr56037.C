// PR c++/56037

struct T
{
  T(int, int);
};

int main()
{
  static const int zero = 0;
  (T(int(zero), int(zero)));
}
