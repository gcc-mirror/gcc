/* This doesn't work on solaris2 for reasons described in PR 6482.  */
/* { dg-do run { xfail *-*-solaris2* } } */
/* { dg-additional-sources "conpr-2a.cc" } */

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
