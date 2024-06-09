/* { dg-do compile } 
   { dg-options "-std=c23" } */

// test that structs of variable size are detected correctly

void f(int n)
{
	struct bar { char buf[n]; };
	struct foo { struct bar y; } a;
	{
		struct bar { char buf[n]; };
		struct foo { struct bar y; } b;

		typeof((1 ? &a : &b)->y) c = { 0 };	/* { dg-error "variable-sized object may not be initialized" } */
	}
}

void g(int n)
{
	struct bar { char buf[n]; };
	struct foo { struct bar y; } a;
	{
		struct bar { char buf[2]; };
		struct foo { struct bar y; } b;

		typeof((1 ? &a : &b)->y) c = { 0 };	// composite type is not of variable size
	}
}

