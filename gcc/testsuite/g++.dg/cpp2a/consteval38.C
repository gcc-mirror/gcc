// PR c++/118525
// { dg-do compile { target c++20 } }

// Make sure we don't ICE
consteval int id (int i) { return i; }

void
g (int i)
{
    1 ? 1 : id (i) ^ 1; // { dg-error "call to consteval function|'i' is not a constant expression" }
}
