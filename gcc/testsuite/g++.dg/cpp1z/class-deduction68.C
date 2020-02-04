// PR c++/92593
// { dg-do compile { target c++17 } }

template<int I>
struct ref_view
{
  ref_view(T) { };		// { dg-error "incomplete" }
};

ref_view r{1};			// { dg-error "no match|deduction failed" }
