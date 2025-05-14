/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

int f(int n)
{
	struct foo { struct { char (*x)[n]; }; } a;
	{
		struct foo { struct { char (*x)[n]; }; } b = a;
	}
}

int g(int n)
{
	struct foo { struct { char (*x)[n]; }; } a;
	{
		struct foo { struct { char (*x)[n]; }; } *b = &a;
	}
}

int h(int n)
{
	struct foo { struct { struct bar { char (*x)[n]; }* p; }; } a;
	{
		struct foo { struct { struct bar { char (*x)[n]; }* p; }; } *b = &a;
	}
}

