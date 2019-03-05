// { dg-do compile { target c++17 } }
// { dg-options "" }

namespace A::B::C
{
	struct X {};
	namespace T::U::V { struct Y {}; }
}

A::B::C::X x;
A::B::C::T::U::V::Y y;

inline namespace D::E {} // { dg-error "cannot be inline" }

namespace F::G:: {} // { dg-error "namespace name required" }

namespace G __attribute ((visibility ("default"))) ::H {} // { dg-error "cannot have attributes" }

namespace H [[deprecated]] ::I {} // { dg-error "cannot have attributes|ignored" }

namespace __attribute ((visibility ("default"))) I::J {} // { dg-error "cannot have attributes" }

namespace [[deprecated]] J::K {} // { dg-error "cannot have attributes|ignored" }

