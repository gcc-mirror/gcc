// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/36741

#include <stddef.h>
const char*
foo()
{
    return new char[~static_cast<size_t>(0)];// { dg-error "size of array" }
}
