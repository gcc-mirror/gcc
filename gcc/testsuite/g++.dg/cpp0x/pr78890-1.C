// PR c++/78890
// { dg-do compile { target c++11 } }

int
main()
{
  union {
    int a;
    int &b = a;		// { dg-error "may not have reference type" }
  };
  a = 1;
  auto c = b + 1;
}
