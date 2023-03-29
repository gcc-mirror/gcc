// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++20 } }
// Test from P2266R1, $ 3.3. Two overload resolutions are overly confusing.

struct Widget {
    Widget();
    Widget(Widget&&);
};

struct Frodo {
    Frodo(Widget&);
    Frodo(Widget&&) = delete;
};

struct Sam {
    Sam(Widget&) = delete; // #1
    Sam(const Widget&);  // #2
};

Sam twelve() {
    Widget w;
    // This is supposed to call #2 since C++20 because P1155.
    // But we actually choose #1 since r11-2411 (in C++20 only).
    return w; // { dg-error "deleted" "" { target c++20_only } }
}

Frodo thirteen() {
    Widget w;
    // This is a correct error in both C++20 and C++23.
    return w;  // { dg-error "use of deleted function" }
}

struct Merry {};
struct Pippin {};
struct Together : Merry, Pippin {};
struct Quest {
    Quest(Merry&&);
    Quest(Pippin&&);
    Quest(Together&);
};

Quest fourteen() {
  Together t;
  // C++20: calls Quest(Together&).  Proposed: ill-formed.
  return t; // { dg-error "ambiguous" "" { target c++23 } }
}
