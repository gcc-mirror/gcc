/* { dg-lto-do link } */

struct X;
struct Y {
    struct X (*fnptr)(struct X);
};
struct Y foo;
