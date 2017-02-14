// PR c++/77804 - Internal compiler error on incorrect initialization of
// new-d array
// { dg-do compile }
// { dg-additional-options "-Wplacement-new -Wvla -Wno-error=vla" }

void* operator new[] (__SIZE_TYPE__ n, void *p) { return p; }

int main()
{
    char buf[256];
    unsigned n = 10;
    int* p = new (buf) (int[n]);  // { dg-warning "non-constant array new length must be specified without parentheses around the type-id" }
    //  { dg-warning "ISO C\\+\\+ forbids variable length array" "vla warning" { target *-*-* } .-1 }
}
