// Bug: g++ can't deal with references to arrays.
// Build don't link:

typedef float Matrix[4][4];
Matrix m;
Matrix& f () { return m; }
