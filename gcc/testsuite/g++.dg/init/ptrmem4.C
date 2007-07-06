// PR c++/32245
// { dg-do run } 

struct foo {
  int mem1;
  int foo::* mem2;
};

int main () {
  foo x = { 0 } ;
  if (x.mem2 != foo().mem2)
    return 1;
}
