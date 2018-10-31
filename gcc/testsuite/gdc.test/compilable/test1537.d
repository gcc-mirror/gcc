// 1537

void foo(char[] s)
{
    int x = -1;

    while (s.length)
    {
        char c = s[0];

        if (c == '}')
           break;

        assert (c >= '0' && c <= '9', s[0..$]);

        if (x == -1)
            x = 0;
    }
}

/**************************************/

enum bug4732 = 42;
static assert( __traits(identifier, bug4732) == "bug4732");

/**************************************/

template Compileable(int z) { bool OK=true;}

int bug5245a(U)()
{
    { enum   T { a = 5  } T v; }
    { enum   T { a = 6  } T w; }
    return 91;
}

int bug5245b(U)()
{
    { struct T { int a = 2; } T v; }
    { union  T { int a = 3; } T w; }
    return 91;
}

int bug5245c(U)()
{
    { struct T { int a = 2; } T v; }
    { class  T { int a = 3; } T w; }
    return 91;
}

int bug5245d(U)()
{
    { enum  T { a = 3 } T w; }
    { struct T { int a = 2; } T v; }
    return 91;
}


static assert(!is(typeof(Compileable!(bug5245a!(int)()).OK)));
static assert(!is(typeof(Compileable!(bug5245b!(int)()).OK)));
static assert(!is(typeof(Compileable!(bug5245c!(int)()).OK)));
static assert(!is(typeof(Compileable!(bug5245d!(int)()).OK)));

/**************************************/

class Bug5349(T) // segfault D2.051
{
    int x;
    static int g()
    {
        class B
        {
            int inner()
            {
                return x; // should not compile
            }
        }
        return (new B).inner();
    }
    int y =  g();
}

static assert(!is(typeof(Bug5349!(int))));

/**************************************/

class Bug4033 {}

class Template4033(T) {
    static assert(is(T : Bug4033));
}

alias Template4033!(Z4033) Bla;

class Z4033 : Bug4033 { }

/**************************************/

struct Bug4322 {
    int[1] a = void;
}

void bug4322() {
    Bug4322 f = Bug4322();
    Bug4322 g = Bug4322.init;
}
