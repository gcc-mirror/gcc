module imports.test49a;

import std.stdio;

int x;

template Foo(T)
{
   static this()
   {
	printf("static this()\n");
	assert(x == 0);
	x++;
   }

   static ~this()
   {
	printf("static ~this()\n");
	assert(x == 1);
	x--;
   }
}

void baz()
{
    alias Foo!(int) bar;
}

