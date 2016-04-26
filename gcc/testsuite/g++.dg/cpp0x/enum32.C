// PR c++/70241
// { dg-do compile { target c++11 } }

class A {
public:
   enum B : int;
};

enum A::B : int {
   x
};

struct C {
private:
    enum D : int;
};

enum C::D : int {
   y
};

int main() {
   A::x;
   C::y; // { dg-error "private" }
}
