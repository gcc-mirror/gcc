// { dg-do assemble  }
// Based on a test case by Koos Vriezen <koos@polder.ubc.kun.nl>

struct foo {
    static void (*mystatic) ();
};

void bar(foo& t) {
    t.mystatic ();
}
