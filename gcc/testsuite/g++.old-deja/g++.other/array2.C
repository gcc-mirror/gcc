// { dg-do run  }
int i;

struct S {
  S () {
    ++i;
  };

  S (int) {
  };
};

int main()
{
  S s[3][3] = { 2 };

  if (i != 8)
    return 1;
}
