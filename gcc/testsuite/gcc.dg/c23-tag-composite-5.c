/* { dg-do compile }
 * { dg-options "-std=c23" } 
 */

// anonymous structs / unions

extern struct foo { int (*x)[]; struct { int y; }; } a;
extern struct foo { int (*x)[]; struct { int y; }; } a;
extern struct bar { int (*x)[]; union { int y; }; } b;
extern struct bar { int (*x)[]; union { int y; }; } b;

void f(void)
{
	struct foo { int (*x)[1]; struct { int y; }; } c;
	extern typeof(*(1 ? &a : &c)) a;
	a.y;

	struct bar { int (*x)[1]; union { int y; }; } d;
	extern typeof(*(1 ? &b : &d)) b;
	b.y;
}


struct foo { int (*x)[]; union { int y; }; };		/* { dg-error "redefinition" } */

