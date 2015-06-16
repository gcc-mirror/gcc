// PR c++/59682

int* p = new() int;  // { dg-error "expected expression-list or type-id" }
