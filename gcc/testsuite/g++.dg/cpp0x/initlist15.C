// { dg-do compile { target c++11 } }

// Just discard errors pointing at header files
// { dg-prune-output "include" }

#include <vector>
#include <typeinfo>

using namespace std;

template< typename ... ArgTypes >
void test( ArgTypes ... args ) {
   vector<type_info*> x = { &typeid(ArgTypes)... }; // { dg-error "" }
}

int main()
{
    test( 1, 3.14f, 2.78 );
    return 0;
}
