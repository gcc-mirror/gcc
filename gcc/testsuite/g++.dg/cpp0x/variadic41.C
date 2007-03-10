// { dg-options "-std=gnu++0x" }
template<typename... Args>
void f(const Args&... args, int oops); // { dg-error "end" }
