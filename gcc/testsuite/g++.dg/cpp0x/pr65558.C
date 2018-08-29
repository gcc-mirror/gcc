// PR c++/65558
// { dg-do compile { target c++11 } }

inline namespace
__attribute__((__abi_tag__)) // { dg-warning "ignoring .abi_tag. attribute on anonymous namespace" }
{
}
