// PRMS Id: 5070 (testcase 2)

int status = 1;

struct foo {
  foo& operator= (const foo&) { status = 0; return *this; }
};

struct xx {
  foo a;
};

struct yy : public xx {
  yy(foo& a) { xx::a = a; }
};

int main()
{
  foo f;
  yy y (f);

  return status;
}
