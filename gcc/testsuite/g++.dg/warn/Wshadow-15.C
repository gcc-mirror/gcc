// { dg-options "-Wshadow" }

template<typename T>
void* operator new(__SIZE_TYPE__, T&);  // { dg-warning "7:shadowing library function" }

template<typename T>
void operator delete(void *, T&);  // { dg-warning "6:shadowing library function" }
