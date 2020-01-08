// PR c++/50956
// { dg-options "-Wcast-qual" }

void* p = (void*)"txt"; // { dg-warning "11:cast" }
