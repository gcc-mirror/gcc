// PR c++/5665
// Origin: Bergur Ragnarsson <bergur@tern.is>
// Reduced version: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

template<typename T> class A
{
    class B { X foo(); }; // { dg-error "" }
};

template<typename T> X A<T>::B::foo() {} // { dg-error "" }
