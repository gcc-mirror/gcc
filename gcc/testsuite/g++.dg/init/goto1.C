// PR c++/14724
// { dg-do run }

int j;

template <class T>
struct C {
  C() { ++j; }
  ~C() { --j; }
};

int main(int, char **) {
  {
    int i = 0;
 again:
    C<int> v;
    if (++i < 10)
      goto again;
  }

  return j;
}

