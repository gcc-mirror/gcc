/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc++-compat" } */

// test that DECL_BIT_FIELD_TYPE is set correctly

enum e { A, B, C };
struct s { enum e m : 3; char (*y)[]; } s = { };

void f(enum e);

void foo ()
{
	struct s { enum e m : 3; char (*y)[1]; } t = { };
	f(s.m);
	f(t.m);
	typeof(*(1 ? &s : &t)) u = { };
	f(u.m);			// should not warn
}

