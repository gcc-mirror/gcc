// { dg-do assemble  }
// Origin: "Marcin 'Qrczak' Kowalczyk" <qrczak@knm.org.pl>

template<template<typename> class t1, typename t0> t1<t0> single()
{
    return single<t1,t0>();
}

template<typename a> class T1 {};
int main()
{
    single<T1,int>();
}

