/* This used to ICE. */
/* { dg-do "compile" } */

struct a {};

class foo : public a, a
{ /* { dg-error "duplicate base type|at end of input" } */
