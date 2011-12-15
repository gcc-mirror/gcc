// Origin PR c++/51473
// { dg-options "-std=c++11" }

struct A
{
    auto friend struct B; // { dg-error "multiple types|can only be specified|friend" }
};

auto int; // { dg-error "multiple types|can only be specified for variables" }
