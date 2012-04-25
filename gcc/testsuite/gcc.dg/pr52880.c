/* PR c/52880 */
/* { dg-do compile } */
/* { dg-options "-Woverride-init" } */

struct A { int a; int b; };
struct B { struct A c; int d, e; };
struct B f = { .c.a = 0, .e = 1, .d = 2, .c.b = 3 };
struct C { int g; int h; };
struct D { int i; struct C j; int k; };
struct D l = { .j.g = 0, .k = 1, .i = 2, .j.h = 3 };
