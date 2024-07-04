/* { dg-do compile }
 * { dg-options "-std=c23" }
 */

// bit-fields

extern struct foo { int x:3; } x;
struct foo { int x:3; } y;

void f()
{
	extern typeof(*(1 ? &x : &y)) x;
	&x.x;					/* { dg-error "bit-field" } */
}


void g()
{
	struct foo { int x:3; } z;
	extern typeof(*(1 ? &x : &z)) x;
	&x.x;					/* { dg-error "bit-field" } */
}

struct foo { int x:2; };			/* { dg-error "redefinition" } */

extern struct bar { int x:3; } a;

void h()
{
	struct bar { signed int x:3; } b;
	extern typeof(*(1 ? &a : &b)) a;	
	&a.x;					/* { dg-error "bit-field" } */
}

void i()
{
	struct bar { unsigned int x:3; } c;
	(1 ? &a : &c);				/* { dg-error "mismatch" } */
}

struct bar { unsigned int x:3; } d;		/* { dg-error "redefinition" } */
struct bar { signed int x:3; } e;		/* { dg-error "redefinition" } */


struct bas { int x:2; int :0; int z:1; };
struct bas { int x:2; int :0; int z:1; };
struct bas { int x:2; int :1; int z:1; };	/* { dg-error "redefinition" } */



