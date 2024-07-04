// PR c++/53220
// { dg-options "" }

int* f() { return (int[]){42}; } // { dg-warning "pointer to temporary" }
