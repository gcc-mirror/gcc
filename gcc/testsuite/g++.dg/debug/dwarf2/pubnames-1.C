// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/39706
// { dg-do compile { target *-*-darwin* } }
// { dg-options "-gdwarf-2 -dA -fno-merge-debug-strings" }
//
// There should be one debug_pubnames section generated.
// { dg-final { scan-assembler-times "\.section\[\t \]\[^\n\]*debug_pubnames" 1 } }
//
// Then check of the presence of the names we are interested in.
// { dg-final { scan-assembler-times "\"main.0\"\[^\n\]*external name" 1 } }
// { dg-final { scan-assembler-times "\"ns::ns_x\[^\n\]*external name" 1 } }
// { dg-final { scan-assembler-times "\"y::y_x\[^\n\]*external name" 1 } }

namespace ns { int ns_x; }
class y { public: static int y_x; };
int y::y_x;
int main() { return ns::ns_x; }
