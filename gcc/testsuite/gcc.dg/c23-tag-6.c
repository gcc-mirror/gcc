/*
 * { dg-do compile }
 * { dg-options "-std=c23" }
 */

// (in-)completeness

struct foo {
	char x[10];
} x;

// complete, same type

struct foo {
	_Static_assert(_Generic(&x, struct foo*: 1, default: 0));
	char x[_Generic(&x, struct foo*: 10, default: 1)];
	_Static_assert(_Generic(0, struct foo: 0, default: 1));
};

// incomplete, same type

struct bar* p;
struct bar {
	_Static_assert(_Generic(p, struct bar*: 1, default: 0));
	char x[_Generic(p, struct bar*: 10, default: 1)];
	_Static_assert(_Generic(0, struct bar: 0, default: 1));	/* { dg-error "incomplete type" } */
};

struct bar {
	char x[10];
};

struct h *hp;

void f(void)
{
	// again incomplete, different type

	struct foo { 
		char x[_Generic(&x, struct foo*: 1, default: 10)]; 
		_Static_assert(_Generic(0, struct foo: 0, default: 1));	/* { dg-error "incomplete type" } */
	};

	struct foo z;
	_Static_assert(10 == sizeof(z.x), "");

	// still incomplete, different type

	struct h { 
		char x[_Generic(hp, struct h*: 1, default: 10)]; 
		_Static_assert(_Generic(0, struct h: 0, default: 1));	/* { dg-error "incomplete type" } */
	};

	struct h y;
	_Static_assert(10 == sizeof(y.x), "");
}


