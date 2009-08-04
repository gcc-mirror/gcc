// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/39706
// { dg-options "-g -dA" }
// { dg-do compile }
// { dg-final { scan-assembler-times ".debug_pubnames" 1 } }
// { dg-final { scan-assembler-times "\"main\".*external name" 1 } }
// { dg-final { scan-assembler-times "\"ns::ns_x.*external name" 1 } }
// { dg-final { scan-assembler-times "\"y::y_x.*external name" 1 } }

namespace ns { int ns_x; }
class y { public: static int y_x; };
int y::y_x;
int main() { return ns::ns_x; }
