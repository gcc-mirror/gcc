// PR c++/82331
// { dg-do compile { target c++17 } }

template <auto>
class X;

template <typename R, typename... A, R (*F) (A...)>
class X<F> {
public:
    static R call (A... args)
    {
        return (*F)(args...);
    }
};

int func (int a, int b) { return a + b; }

int test () { return X<&func>::call(1, 2); }
