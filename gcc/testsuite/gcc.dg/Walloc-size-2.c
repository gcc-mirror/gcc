/* Test that VLA types do not crash with -Walloc-size
   { dg-do compile }
   { dg-options "-Walloc-size" }
 * */

struct foo { int x[10]; };

void fo0(int n)
{
        struct foo (*p)[n] = __builtin_malloc(sizeof *p);
}

void fo1(int n)
{
	struct bar { int x[n]; };
	struct bar *p = __builtin_malloc(sizeof *p);
}

