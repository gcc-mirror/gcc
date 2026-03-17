/* { dg-do run } */
/* { dg-options "-O2 -std=c23" } */


/* Based on the submitted test case for PR123356 but using types with
 * tags and moving the second version into another scope.  */

struct foo { long x; };

void f()
{
	struct foo { };
	struct bar { struct foo *c; };
	union baz { struct foo *c; };
	struct arr { struct foo *c[1]; };
	struct fun { struct foo (*c)(); };
}


void f1()
{
	struct foo { };
	struct bar { struct foo *c; };
	union baz { struct foo *c; };
	struct arr { struct foo *c[1]; };
	struct fun { struct foo (*c)(); };
}

struct bar { struct foo *c; };
union baz { struct foo *c; };
struct arr { struct foo *c[1]; };
struct fun { struct foo (*c)(); };

void f2()
{
	struct foo { int y; };
	struct bar { struct foo *c; };
	union baz { struct foo *c; };
	struct arr { struct foo *c[1]; };
	struct fun { struct foo (*c)(); };
}

__attribute__((noinline))
struct foo * g1(struct bar *B, struct bar *Q)
{
    struct bar t = *B;
    *B = *Q;
    *Q = t;
    return B->c;
}

__attribute__((noinline))
struct foo * g2(union baz *B, union baz *Q)
{
    union baz t = *B;
    *B = *Q;
    *Q = t;
    return B->c;
}

__attribute__((noinline))
struct foo { long x; } * 
	g3(struct bar { struct foo { long x; } *c; } *B,
	   struct bar { struct foo { long x; } *c; } *Q)
{
    struct bar t = *B;
    *B = *Q;
    *Q = t;
    return B->c;
}

__attribute__((noinline))
struct foo * g4(struct arr *B,
	        struct arr *Q)
{
    struct arr t = *B;
    *B = *Q;
    *Q = t;
    return B->c[0];
}

__attribute__((noinline))
struct foo (*g5(struct fun *B,
	        struct fun *Q))()
{
    struct fun t = *B;
    *B = *Q;
    *Q = t;
    return B->c;
}

struct foo Bd() { };
struct foo Qd() { };

int main()
{
    struct foo Bc = { };
    struct foo Qc = { };

    struct bar B = { &Bc };
    struct bar Q = { &Qc };

    if (g1(&B, &Q) != &Qc)
	    __builtin_abort();

    union baz Bu = { &Bc };
    union baz Qu = { &Qc };

    if (g2(&Bu, &Qu) != &Qc)
	    __builtin_abort();

    struct bar B2 = { &Bc };
    struct bar Q2 = { &Qc };

    if (g3(&B2, &Q2) != &Qc)
	    __builtin_abort();

    struct arr Ba = { &Bc };
    struct arr Qa = { &Qc };

    if (g4(&Ba, &Qa) != &Qc)
	    __builtin_abort();
#if 0
    // PR114959
    struct fun Bf = { &Bd };
    struct fun Qf = { &Qd };

    if (g5(&Bf, &Qf) != &Qd)
	    __builtin_abort();
#endif
    return 0;
}

