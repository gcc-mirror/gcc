struct S {
    ~this() {}
}

__gshared S s = S();

ref S refS() {
    return s;
}

void takeS(S s2) {
    assert(&s !is &s2);
}

void main()
{
    takeS(refS());
}
