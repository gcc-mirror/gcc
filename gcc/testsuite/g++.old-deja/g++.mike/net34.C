// { dg-do run  }
// { dg-options "" }

#include <iostream>

class foo {
public:
        foo(int i) {k = i;}
protected:
        int k;
};

class bar_1 : public foo {
public:
        bar_1(int i) : foo(i) {}
        int get_k() {return k;}
};

class bar_2 : public foo {
public:
    bar_2(int i) : foo(i) {}
        int get_k() {return k;}
};

class multiple : public bar_1, public bar_2 {
public:
        multiple(int i1, int i2) : bar_1(i1), bar_2(i2) {}
        void print() {
                std::cout << "bar_1::k -> " << bar_1::k << "\n";
                std::cout << "bar_2::k -> " << bar_2::k << "\n";
                std::cout << "bar_1::get_k() -> " << bar_1::get_k() << "\n";
                std::cout << "bar_2::get_k() -> " << bar_2::get_k() << "\n";
        }
};

int main() {
        multiple m(1,2);
        m.print();
}


