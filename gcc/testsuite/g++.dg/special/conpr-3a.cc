/* { dg-do run } */

class foo_t {
    int x;
    static int count;
public:
    foo_t(void) { x=++count; }
    int get(void) { return x; }
};

foo_t foo1 __attribute__((init_priority(6000)));
