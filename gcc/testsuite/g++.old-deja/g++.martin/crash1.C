// { dg-do assemble  }
int i = 4;
struct S{
  char c[i];      // { dg-error "" } size not constant
  int h;
  int foo(){
    return h;
  }
};

int main()
{
  S x;
  int i = x.foo();
}
