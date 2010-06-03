// { dg-do run }

#include <new>
    
void * operator new[](std::size_t, std::nothrow_t const &) throw()
{ return 0; }

struct X {
    struct Inner { ~Inner() {} };

    X() {
      Inner * ic = new (std::nothrow) Inner[1]; // SegFault here
    }
};

int main() {
   X table;
}
