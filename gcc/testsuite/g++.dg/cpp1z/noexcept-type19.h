#pragma GCC system_header

typedef decltype(sizeof(0)) size_t;
extern "C" void *malloc (size_t) throw();
