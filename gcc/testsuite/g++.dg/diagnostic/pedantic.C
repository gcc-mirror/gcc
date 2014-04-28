// { dg-do compile }
// { dg-options "-pedantic" }
typedef void F(void);

void foo()
{
    void* p = 0;
    F* f1 = reinterpret_cast<F*>(p);    // { dg-warning "ISO" }
#pragma GCC diagnostic ignored "-pedantic"
    F* f2 = reinterpret_cast<F*>(p);
}
