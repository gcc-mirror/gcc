// PR c++/67829

template<class> class Purr;

template<class, template<class> class>
class Meow;

template<template<class> class P>
class Meow<P<int>, P> { }; // 1

template<template<class> class P, class T>
class Meow<P<T>, P>; // 2

Meow<Purr<int>, Purr> kitty;
