// Build don't link:
// Origin: Theodore Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

class A {
public:
        template <class T> T& f(T& t) const;
};

class B {
public:
        template <class T> T& f(T& t) const;
};

class C: public A,B {
public:
        template <class T> T& f(T& t) const;
};
