// Bug: g++ doesn't generate default constructor.

class A {
public:
  int i;
};

extern "C" int printf(const char *, ...);

int main () {
  A a;
  a.i = 1;
  A b (a);
  printf("%d\n",b.i);
}
