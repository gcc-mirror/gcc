// PR c++/37234
// { dg-do link { target c++11 } }

template <typename T>
class foo {
  public:
    foo() =default;
    ~foo();
};

template <typename T>
foo<T>::~foo() =default;

int main() {

    foo<int> fi;

    return 0;
}
