// PR c++/67829

template<class> class Purr;

template<template<class> class, class, class>
class Meow;

template<template<class> class P>
class Meow<P, P<int>, int> { }; // 1

template<template<class> class P, class T>
class Meow<P, P<int>, T>; // 2

Meow<Purr, Purr<int>, int> kitty;
