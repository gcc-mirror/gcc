struct CPU {
    typedef int (*pfun)();

    template <pfun step1>
    static int dispatch();
};

template<int>
static int foo();

template int CPU::dispatch<&template foo<2> > (); // { dg-error "" }
