/* PR c++/29731. This used to ICE in uses_template_parms. */

template<int> struct A {};

A<({})> a; /* { dg-error "forbids braced-groups within expressions|statement-expressions|template argument 1 is invalid|invalid type" } */
