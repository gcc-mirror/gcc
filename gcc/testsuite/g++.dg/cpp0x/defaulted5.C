// PR c++/37234
// { dg-do link }
// { dg-options "-std=c++11" }

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
