/* { dg-do run } */
/* { dg-additional-sources "conpr-3a.cc conpr-3b.cc" } */

#include <stdlib.h>

class foo_t {
    int x;
    static int count;
public:
    foo_t(void) { x=++count; }
    int get(void) { return x; }
};

int foo_t::count;

extern foo_t foo1, foo2;

int main(void) {

    if ( (foo1.get() != 2) || (foo2.get() != 1) )
        abort();
    exit(0);
}
