// PR c++/70383
// { dg-do compile { target c++11 } }

void meow() {
    void purr();
    void (&f)() = purr;
    [f]{};
}
