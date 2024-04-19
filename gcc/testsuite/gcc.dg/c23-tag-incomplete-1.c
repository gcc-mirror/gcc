/* { dg-do compile }
 * { dg-options "-std=c23 -g" } */

struct a;
typedef struct a b;

void g() {
    struct a { b* x; };
}

struct a { b* x; };
