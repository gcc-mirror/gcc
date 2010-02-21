// Origin: PR c++/42824
// { dg-do compile }

template<int T>
class int_ {
};

template<int T, int T2>
class Unit {
public:
    Unit(const Unit<T, T2>& other) {}
};

template<int T>
class Quan {
public:
    Quan(void) {}

    template<int T2>
    Quan(double value, Unit<T, T2> unit) {}
};
typedef Quan<0> Scalar;

template<int T>
class hlp {
public:
   typedef Quan<T> type;
};

class Mtrl {
public:
    template<int T>
    struct AssoType {
        typedef typename hlp<T>::type type;
    };
};

template<class T>
class Eval {
public:
    Eval(const T& object){}

    template<int V>
    void eval() {
        eval<V> (int_<0>());
    }
private:
    template<typename U> struct Wrap {};

    template<int V, int V2>
    void value(Wrap<Quan<V2> >) {}

    template<int V>
    void value(Wrap<Scalar>) {}

    template<int V>
    void eval(int_<0>) {
        typedef typename T::template AssoType<V>::type Type;
        value<V>(Wrap<Type>());
    }
};

class Foo {
public:
    static void eval(const Mtrl& mtrl) {
        Eval<Mtrl> h(mtrl);
        h.eval<0> ();
    }
};

