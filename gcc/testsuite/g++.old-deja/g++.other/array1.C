int i;

struct S {
  S (int) {
    ++i;
    if (i == 3)
      throw 3;
  };

  S () {}

  ~S() {
    --i;
  }
};

int main()
{
  try {
    S s[5] = { 0, 1, 2, 3, 4 };
  } catch (...) {
  }

  if (i != 1)
    return 1;
}
