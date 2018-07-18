// Testcase from P0136
// { dg-do compile { target c++11 } }

struct M { M(); M(int); };
struct N : M { using M::M; };
struct O : M {};
struct P : N, O { using N::N; using O::O; };
P p(0); // OK: use M(0) to initialize N's base class,
        // use M() to initialize O's base class
