struct X {
    X ();
};

template <int> struct O {
    struct I {
        I (const X & = X());
    };
};
template struct O<2>;
