// PR c++/50830
// { dg-do compile { target c++11 } }

template<template<class> class...>
struct list_templates {};

template<class>
struct aa {};

template<class... T>
struct test {};

template<template<class> class... F, class T>
struct test<list_templates<F...>, T>
{
    struct inner {};
};

test<list_templates<aa>, int> a4; // error
