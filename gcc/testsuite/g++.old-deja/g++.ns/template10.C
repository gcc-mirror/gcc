// Build don't link:
// Origin: Manuel Menezes de Sequeira <mms@torga.iscte.pt>

namespace N {
  template <class T> void g() {}
}
void (*pf)() = N::g<int>;
