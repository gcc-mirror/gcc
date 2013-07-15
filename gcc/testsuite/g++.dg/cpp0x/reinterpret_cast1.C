// PR c++/57869
// { dg-do compile { target c++11 } }

void* po = 0;
void (*pf)() = reinterpret_cast<decltype(pf)>(po);
static_assert(sizeof(po) >= sizeof(pf), "Conversion not supported");
