struct A { };
struct B : public A { };
struct X {
        operator B();
};
X x;

int main()
{
  const A& r = x;
  return 0;
}
