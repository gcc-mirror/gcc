// PRMS Id: 4329
// Bug: default op= gives an warning about casting away volatile.
// Build don't link:

struct foo
{
    volatile int bar[2];
};
