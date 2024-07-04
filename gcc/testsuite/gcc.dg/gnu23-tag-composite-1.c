/* { dg-do compile }
 * { dg-options "-std=c23" } 
 */

// packed structs

struct foo {
	char a;
	int b [[gnu::packed]];
	char d;
	int c [[gnu::packed]];
};

struct foo {
	char a;
	int b [[gnu::packed]];
	char d;
	int c [[gnu::packed]];
};

extern struct foo x;

void g()
{
	struct foo {
		char a;
		int b [[gnu::packed]];
		char d;
		int c [[gnu::packed]];
	};

	extern struct foo y;
	extern typeof(*(1 ? &x : &y)) x;
}

void h()
{
	struct foo {
		char a;
		int b;
		char d;
		int c;
	}* z = &x;		/* { dg-error "incompatible" } */
}

