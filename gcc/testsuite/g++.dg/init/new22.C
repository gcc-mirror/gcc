// PR c++/29001

void* operator new (__SIZE_TYPE__) { return; } // { dg-error "with no value" }
