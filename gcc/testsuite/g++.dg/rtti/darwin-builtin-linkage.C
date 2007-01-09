/* { dg-do compile { target *-*-darwin* } } */
/* { dg-final { scan-assembler-not "\\.weak_definition __ZTI" } } */

/* Verify that none of the type_info structures for the fundamental
   types are emitted as weak on Darwin.  */

#include <cxxabi.h>

namespace __cxxabiv1 {

using namespace std;

// This has special meaning to the compiler, and will cause it
// to emit the type_info structures for the fundamental types which are
// mandated to exist in the runtime.
__fundamental_type_info::
~__fundamental_type_info ()
{}
 
}
