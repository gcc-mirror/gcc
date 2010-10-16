// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/39706
// { dg-do compile { target *-*-darwin* } }
// { dg-options "-g -dA -fno-merge-debug-strings" }
//
// There should be one debug_pubnames section generated.
// On Darwin though, there is also a label pointing at the begining of the
// debug_pubnames section. The assembly code of that label adds an occurence
// of section declaration assembly. So on Darwin, we need to check for two
// occurences of the debug_pubnames section declaration.
// { dg-final { scan-assembler-times "\.section\[\t \]\[^\n\]*debug_pubnames" 1 { target { ! *-*-darwin* } } } }
// { dg-final { scan-assembler-times "\.section\[\t \]\[^\n\]*debug_pubnames" 2 { target { *-*-darwin* } } } }
//
// Then check of the presence of the names we are interested in.
// { dg-final { scan-assembler-times "\"main.0\"\[^\n\]*external name" 1 } }
// { dg-final { scan-assembler-times "\"ns::ns_x\[^\n\]*external name" 1 } }
// { dg-final { scan-assembler-times "\"y::y_x\[^\n\]*external name" 1 } }

namespace ns { int ns_x; }
class y { public: static int y_x; };
int y::y_x;
int main() { return ns::ns_x; }
