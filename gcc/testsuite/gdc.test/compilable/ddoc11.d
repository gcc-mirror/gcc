// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

/// The various floating point exceptions
enum
{
    FE_INVALID      = 1,        ///
    FE_DENORMAL     = 2,        ///
    FE_DIVBYZERO    = 4,        ///
    FE_OVERFLOW     = 8,        ///
    FE_UNDERFLOW    = 0x10,     ///
    FE_INEXACT      = 0x20,     ///
    FE_ALL_EXCEPT   = 0x3F,     /// Mask of all the exceptions
}

alias int myint;

///
myint bar;

///
myint foo(myint x = myint.max)
{
    return x;
}


///
class Foo
{
    ///
    this(string s) { }
}


extern (C):

///
struct div_t { int  quot,rem; }
///
struct ldiv_t { int quot,rem; }
///
struct lldiv_t { long quot,rem; }

    div_t div(int,int);	///
    ldiv_t ldiv(int,int); ///
    lldiv_t lldiv(long, long); ///



    void *calloc(size_t, size_t);	/// 
    void *malloc(size_t);	/// dittx

/**
Example:
---
private:
    int i = 0;
---
*/
void test1()
{
}





