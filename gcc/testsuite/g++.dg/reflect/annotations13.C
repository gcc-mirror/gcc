// PR c++/124399
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

[[=1]] int a [[=2]], b [[=3]];
static_assert ([: constant_of (annotations_of (^^a)[0]) :] == 1);
static_assert ([: constant_of (annotations_of (^^a)[1]) :] == 2);
static_assert ([: constant_of (annotations_of (^^b)[0]) :] == 1);
static_assert ([: constant_of (annotations_of (^^b)[1]) :] == 3);

[[=10, =11]] extern int e;
[[=1]] [[=2, =3]] int c [[=4, =5]] [[=6]], d [[=7, =8]] (), e [[=9]];
[[=12]] extern int c;
static_assert ([: constant_of (annotations_of (^^c)[0]) :] == 1);
static_assert ([: constant_of (annotations_of (^^c)[1]) :] == 2);
static_assert ([: constant_of (annotations_of (^^c)[2]) :] == 3);
static_assert ([: constant_of (annotations_of (^^c)[3]) :] == 4);
static_assert ([: constant_of (annotations_of (^^c)[4]) :] == 5);
static_assert ([: constant_of (annotations_of (^^c)[5]) :] == 6);
static_assert ([: constant_of (annotations_of (^^c)[6]) :] == 12);
static_assert ([: constant_of (annotations_of (^^d)[0]) :] == 1);
static_assert ([: constant_of (annotations_of (^^d)[1]) :] == 2);
static_assert ([: constant_of (annotations_of (^^d)[2]) :] == 3);
static_assert ([: constant_of (annotations_of (^^d)[3]) :] == 7);
static_assert ([: constant_of (annotations_of (^^d)[4]) :] == 8);
static_assert ([: constant_of (annotations_of (^^e)[0]) :] == 10);
static_assert ([: constant_of (annotations_of (^^e)[1]) :] == 11);
static_assert ([: constant_of (annotations_of (^^e)[2]) :] == 1);
static_assert ([: constant_of (annotations_of (^^e)[3]) :] == 2);
static_assert ([: constant_of (annotations_of (^^e)[4]) :] == 3);
static_assert ([: constant_of (annotations_of (^^e)[5]) :] == 9);
