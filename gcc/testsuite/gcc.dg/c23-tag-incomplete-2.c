/* { dg-do compile }
 * { dg-options "-std=c23 -g" } */

struct a;
typedef struct a b;

void f() {
	extern struct a { b* x; } t;
}

extern struct a { b* x; } t;
