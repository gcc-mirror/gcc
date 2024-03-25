/* { dg-do compile }
 * { dg-options "-std=c23" } 
 */

// alignment

extern struct foo { char x; alignas(int) char y; } a;
extern struct foo { char x; alignas(int) char y; } a;

void f()
{
	extern struct foo { char x; alignas(int) char y; } b;
	extern typeof(*(1 ? &a : &b)) a;
	static_assert(alignof(a.y) == alignof(int));
}



