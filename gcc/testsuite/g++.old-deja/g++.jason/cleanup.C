// { dg-do run  }
// Bug: continue over object decl calls destructor but not constructor.

int c = 0;
int d = 0;
extern "C" int printf(const char *,...);

class Foo {
public:
  Foo(){ c++; }
  ~Foo(){ d++; }
};

int main()
{
  for(int i=0;i<2;i++){
    continue;
    Foo bar;
  }
  printf ("%d %d\n", c, d);
  if (c == d && d == 0)
    return 0;
  return 1;
}
