struct X { int i; };
struct Y {
    struct X (*fnptr)(struct X);
};
extern struct Y foo;
int main()
{
  struct X x;
  foo.fnptr(x);
  return 0;
}
