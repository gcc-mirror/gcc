// PR c++/62212
// { dg-do assemble }

typedef int my_int;

template<typename T>
struct X {
    enum {value = 1};
};

template<typename T>
void f(const my_int(&)[X<T>::value]);

int main() {
    const my_int a[1] = {};
    f<void>(a);
}

// { dg-final { scan-assembler "_Z1fIvEvRAsr1XIT_E5value_Ki" } }
