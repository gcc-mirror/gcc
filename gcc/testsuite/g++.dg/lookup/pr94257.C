// { dg-do compile { target c++11 } }
// PR94257 ICE with silly inline nest.

inline namespace B { // { dg-message "namespace B { }" }
namespace B { }  // { dg-message "namespace B::B { }" }
}

namespace B { // { dg-error ".namespace B. is ambiguous" }
}

// But this is fine
namespace D {
inline namespace D { }
}
namespace D {
}
