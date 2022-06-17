/* PR c/84685 */
/* { dg-do compile } */
/* { dg-options "-Wmissing-field-initializers" } */

struct T
{
    int a;
    int *b;
    int c;
    int d;
    int *e;
    int f;
    int g;
    int h;
};

struct T foo(int bar);

struct T foo(int bar)
{
    struct T t = { .b = (int[]){ 1 }, .e = (int[]){ 2 } }; /* { dg-bogus "missing initializer" } */
    t.c = bar;
    return t;
}
