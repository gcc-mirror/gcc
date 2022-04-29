/* Test that -Wparentheses warns for struct/class assignments,
   except for explicit calls to operator= (). */
/* PR c++/25689 */
/* { dg-options "-Wparentheses" }  */

struct A
{
	A& operator= (int);
	A operator= (double);
	operator bool ();
};

struct B
{
	bool x;
	B& operator= (int);
	B operator= (double);
	operator bool ();
};

struct C
{
	C& operator= (int);
	virtual C operator= (double);
	operator bool ();
};

/* Test empty class */
void f1 (A a1, A a2)
{
	if (a1 = 0); /* { dg-warning "suggest parentheses" } */
	if (a1 = 0.); /* { dg-warning "suggest parentheses" } */
	if (a1.operator= (0));
	if (a1.operator= (a2));

	/* Ideally, we'd warn for empty classes using trivial operator= (below),
	   but we don't do so yet as it is a non-trivial COMPOUND_EXPR. */
	if (a1 = a2); /* { dg-warning "suggest parentheses" "" { xfail *-*-* } } */
}

/* Test non-empty class */
void f2 (B b1, B b2)
{
	if (b1 = 0); /* { dg-warning "suggest parentheses" } */
	if (b1 = 0.); /* { dg-warning "suggest parentheses" } */
	if (b1 = b2); /* { dg-warning "suggest parentheses" } */
	if (b1.operator= (0));
	if (b1.operator= (b2));
}

/* Test class with vtable */
void f3 (C c1, C c2)
{
	if (c1 = 0); /* { dg-warning "suggest parentheses" } */
	if (c1 = 0.); /* { dg-warning "suggest parentheses" } */
	if (c1 = c2); /* { dg-warning "suggest parentheses" } */
	if (c1.operator= (0));
	if (c1.operator= (c2));
}
