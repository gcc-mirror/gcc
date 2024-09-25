/* { dg-options "--coverage -fcondition-coverage -std=c++11" } */
/* { dg-do run { target native } } */
/* { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } } */

#include <vector>
#include <stdexcept>

class nontrivial_destructor
{
public:
    explicit nontrivial_destructor (int v) : val (v) {}
    ~nontrivial_destructor () {}

    explicit operator bool() const { return bool(val); }

    int val;
};

int identity (int x) { return x; }
int throws (int) { throw std::runtime_error("exception"); }

int
throw_if (int x)
{
    if (x) /* conditions(1/2) true(0) */
	   /* conditions(end) */
	throw std::runtime_error("exception");
    return x;
}

/* Used for side effects to insert nodes in conditional bodies etc.  */
int x = 0;

/* Conditionals work in the presence of non-trivial destructors.  */
void
mcdc001a (int a)
{
    nontrivial_destructor v (a);

    if (v.val > 0) /* conditions(2/2) */
	x = v.val;
    else
	x = -v.val;
}

/* Non-trivial destructor in-loop temporary.  */
nontrivial_destructor
mcdc002a (int a, int b)
{
    for (int i = 0; i < a; i++) /* conditions(2/2) */
    {
	nontrivial_destructor tmp (a);
	if (tmp.val % b) /* conditions(2/2) */
	    return nontrivial_destructor (0);
	x += i;
    } /* conditions(suppress) */
      /* conditions(end) */

    return nontrivial_destructor (a * b);
}

/* Conditional in constructor.  */
void
mcdc003a (int a)
{
    class C
    {
    public:
	explicit C (int e) : v (e)
	{
	    if (e) /* conditions(1/2) false(0) */
		v = x - e;
	}
	int v;
    };

    C c (a);
    if (c.v > 2) /* conditions(1/2) true(0) */
		 /* conditions(end) */
	x = c.v + a;
}

/* Conditional in destructor.  */
void
mcdc004a (int a)
{
    class C
    {
    public:
	explicit C (int e) : v (e) {}
	~C ()
	{
	    if (v) /* conditions(2/2) */
		x = 2 * v;
	}
	int v;
    };

    C c (a);
    x = 1; // arbitrary action between ctor+dtor
}

/* Conditional in try. */
void
mcdc005a (int a)
{
    try
    {
	if (a) /* conditions(1/2) true(0) */
	       /* conditions(end) */
	    x = 2 * identity (a);
	else
	    x = 1;
    }
    catch (...)
    {
	x = 0;
    }
}

/* Conditional in catch.  */
void
mcdc006a (int a) {
    try
    {
	throws (a);
    }
    catch (std::exception&)
    {
	if (a) /* conditions(1/2) false(0) */
	       /* conditions(end) */
	    x = identity (a);
	else
	    x = 0;
    }
}

void
mcdc006b (int a)
{
    if (a) /* conditions(1/2) true(0) */
	   /* conditions(end) */
	throws (a);
    else
	x = 1;
}

void
mcdc006c (int a) try
{
    throws (a);
}
catch (...) {
    if (a) /* conditions(2/2) */
	x = 5;
}

/* Temporary with destructor as term.  */
void
mcdc007a (int a, int b)
{
    x = a && nontrivial_destructor (b); /* conditions(3/4) false(1) destructor() */
}

void
mcdc007b (int a, int b)
{
    if (a || throw_if (b)) /* conditions(3/4) true(1) destructor() */
	x = -1;
    else
	x = 1;
}

void
mcdc007c (int a, int b)
{
    if (throw_if (a) || throw_if (b)) /* conditions(2/4) true(0 1) destructor() */
	x = -1;
    else
	x = 1;
}

/* Destructor with delete.  */
void
mcdc008a (int a)
{
    class C
    {
    public:
        int size = 5;
        int* ptr = nullptr;

        explicit C (int v) : size (v + 5), ptr (new int[size]) /* conditions(suppress) */
							       /* conditions(end) */
        {
	    for (int i = 0; i < size; i++) /* conditions(2/2) */
		ptr[i] = i + 1;
        }
        ~C()
        {
	    // delete with implicit nullptr check
	    delete ptr; /* conditions(1/2) false(0) */
			/* conditions(end) */
	}
    };

    C c (a);
    if (c.ptr[a + 1]) /* conditions(1/2) false(0) */
	x = a;
}

/* Templates.  */
template <typename T>
void
mcdc009a (T a)
{
    if (a > 0) /* conditions(1/2) false(0) */
	       /* conditions(end) */
	x += 2;
}

/* constexpr.  */

/* Compile-time evaluated branches do not contribute to coverage.  */
constexpr int
mcdc010a (int a, int b)
{
    return a > b ? 1 : 2; /* conditions(1/2) true(0) */
			  /* conditions(end) */
}

/* Compile-time only evaluated functions do not show up in the compiled program
   and gets no coverage at all.  If this would generate output unexpectedly it
   would trigger a test failure ("unexpected output").  */
constexpr int
mcdc010b (int a, int b)
{
    return a > b ? 1 : 2;
}

int
main (void)
{
    mcdc001a (0);
    mcdc001a (1);

    mcdc002a (1, 1);
    mcdc002a (1, 2);

    mcdc003a (1);

    mcdc004a (0);
    mcdc004a (1);

    mcdc005a (0);

    mcdc006a (1);

    mcdc006b (0);

    mcdc006c (0);
    mcdc006c (1);

    mcdc007a (0, 0);
    mcdc007a (1, 1);

    mcdc007b (0, 0);
    mcdc007b (1, 0);

    mcdc007c (0, 0);

    mcdc008a (1);

    mcdc009a (1);

    /* Use identity () so that this is not constexpr eval'd. */
    int v1 = mcdc010a (identity (2), 4);
    constexpr int v2 = mcdc010a (4, 2);

    constexpr int v3 = mcdc010b (2, 4);
}

/* { dg-final { run-gcov conditions { --conditions gcov-18.C } } } */
