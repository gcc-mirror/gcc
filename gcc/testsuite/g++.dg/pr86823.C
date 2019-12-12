// { dg-do compile }

struct X {
private:
  template<typename T>
  struct Y {
    int data;
  };
public:
  int value;
};

int main() {
  typename X::Y<int> a; // { dg-error "private" }
}
