// { dg-do assemble  }
// Bug: g++ can't deal with references to arrays.

typedef float Matrix[4][4];
Matrix m;
Matrix& f () { return m; }
