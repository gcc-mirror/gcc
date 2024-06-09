/* { dg-do compile }
 * { dg-options "-std=c23" } 
 */

// attributes

struct [[gnu::designated_init]] buf { char x; };

struct buf s = { 0 };				/* { dg-warning "positional" } */

void j()
{
	struct buf { char x; } t = { 0 };
	typeof(*(1 ? &s : &t)) u = { 0 };	/* { dg-warning "positional" } */
	typeof(*(1 ? &t : &s)) v = { 0 };	/* { dg-warning "positional" } */
}


struct bar { struct buf y; };
extern struct bar a;
struct bar a = { { 0 } };			/* { dg-warning "positional" } */

void k()
{
	struct buf { char x; } t = { 0 };
	struct bar { struct buf y; } b;
	extern typeof(*(1 ? &a : &b)) a;
	typeof(*(1 ? &a : &b)) c = { { 0 } };	/* { dg-warning "positional" } */
}

