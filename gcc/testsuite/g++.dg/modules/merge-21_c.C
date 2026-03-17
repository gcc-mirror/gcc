// PR c++/124431
// { dg-additional-options "-fmodules" }
// { dg-module-cmi C }

export module C;
export import A;
mat<4> from_triangle_frame;

template struct S<0>;
