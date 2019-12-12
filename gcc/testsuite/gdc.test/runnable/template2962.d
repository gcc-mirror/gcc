// EXTRA_SOURCES: imports/template2962a.d

// comment 29
void foo(T)(T p)
{
    void inner(U)() {
        auto p2 = p;
    }
    inner!int();
}

// comment 20
void funcD(alias x)() {
   assert(x==1.0);
}

void funcC(T)(double a){
    // Case 1: ICE(glue.c)
    funcD!(a)();

    // Case 2: wrong code
    double b = 1.0; funcD!(b)();
}

void bug2962comment36()(int p)
{
    int inner()() { return p; }
    alias inner!() finner;
}

