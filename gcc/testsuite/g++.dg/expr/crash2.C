// PR c++/14267
 
class foo {
public: static int& x;
};
int temp;
int& foo::x=temp;

int main() {
  int x = 3;
  &foo::x = x; // { dg-error "" }
  return 0;
}

