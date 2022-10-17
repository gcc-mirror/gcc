// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++23 } }
// Test from P2266R1, $ 3.4. A specific case involving reference_wrapper.

#include <functional>

struct Widget {
    Widget();
    Widget(Widget&&);
};

std::reference_wrapper<Widget> fifteen() {
    Widget w;
    // OK until CWG1579; OK after LWG2993.  Proposed: ill-formed
    return w;  // { dg-error "could not convert" }
}
