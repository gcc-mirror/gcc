// REQUIRED_ARGS: -w

class F { }

int test1() {
    scope F f = new F(); // comment out and warning goes away
    return 0;
}

int test2() { // no return at end of function
    try {
        return 0;
    } finally { }
}

void main()
{
    test1();
    test2();
}
