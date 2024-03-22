/*
 * { dg-do compile }
 * { dg-options "-std=c23" }
 */

// allowed and forbidden redefinitions of the same struct/union in the same scope

typedef struct p { int a; } pd_t;
typedef struct p { int a; } pd_t;

typedef struct bar { int x; } X;
typedef struct bar { float x; } Y; /* { dg-error "redefinition of struct or union" } */

void test(void)
{
	struct foo { int x; };
	struct foo { float x; }; /* { dg-error "redefinition of struct or union" } */
}

struct aa { int a; };

void f(void)
{
	typedef struct aa A;
	struct bb { struct aa a; } x;
	struct aa { int a; };
	typedef struct aa A;		/* { dg-error "redefinition" } */
	struct bb { struct aa a; } y; /* { dg-error "redefinition of struct or union" } */
	(void)x; (void)y;
}



void h(void)
{
	struct a2 { int a; };
	{
		typedef struct a2 A;
		struct b2 { struct a2 a; } x;
		struct a2 { int a; };
		typedef struct a2 A;		/* { dg-error "redefinition" } */
		struct b2 { struct a2 a; } y; /* { dg-error "redefinition of struct or union" } */
		(void)x; (void)y;
	}
}


union cc { int x; float y; } z;
union cc { int x; float y; } z1;
union cc { float y; int x; } z2;	/* { dg-error "redefinition of struct or union" } */

void g(void)
{
	struct s { int a; };
	struct s { int a; } x0;
	struct p { struct s c; } y1 = { x0 };
	struct p { struct s { int a; } c; } y = { x0 };
}

struct q { struct { int a; }; }; 
struct q { struct { int a; }; }; 
struct q { int a; };  /* { dg-error "redefinition of struct or union" } */

struct r { int a; char b[]; };
struct r { int a; char b[]; };
struct r { int a; char b[1]; }; /* { dg-error "redefinition of struct or union" } */

