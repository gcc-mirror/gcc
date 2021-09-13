// PR c++/67829

template<class> class Purr;

template<template<class> class, class>
class Meow;

template<template<class> class P>
class Meow<P, P<int> > { }; // 1

template<template<class> class P, class T>
class Meow<P, P<T> >; // 2

Meow<Purr, Purr<int> > kitty;
