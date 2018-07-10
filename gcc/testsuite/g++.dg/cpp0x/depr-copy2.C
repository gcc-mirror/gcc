// PR c++/86342
// { dg-options -Wdeprecated-copy }

# 1 "deprcopy.cc"
# 1 "deprcopy.h" 1 3

struct X {
  X() { }
  ~X() { }
};
# 2 "deprcopy.cc" 2

int main()
{
  X x;
  X y = x;
}
