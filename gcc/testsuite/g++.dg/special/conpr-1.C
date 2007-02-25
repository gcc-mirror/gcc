/* { dg-do run { target init_priority } } */

#include <stdlib.h>

class foo_t {
    int x;
public:
    foo_t(void) { x=1; }
    int get(void) { return x; }
};

static foo_t foo __attribute__((init_priority(5000)));

int main(void) {

    if (foo.get())
        exit(0);
    else
        abort();
}
