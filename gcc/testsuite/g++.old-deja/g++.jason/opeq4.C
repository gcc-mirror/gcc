// { dg-do assemble  }
// PRMS Id: 4329
// Bug: default op= gives an warning about casting away volatile.

struct foo
{
    volatile int bar[2];
};
