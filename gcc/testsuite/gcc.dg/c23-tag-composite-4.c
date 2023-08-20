/* { dg-do compile }
 * { dg-options "-std=c23" } 
 */

// conditional operator

void f(void)
{
	struct foo { int x; } a;
	struct foo { int x; } b;
	1 ? a : b;
}

struct bar { int x; } a;

void g(void)
{
	struct bar { int x; } b;
	1 ? a : b;
}

