//PR c++/27316

struct A {};

struct B : A
!               // { dg-error "token" }
{};

struct B : A
!               // { dg-error "token" }
{};
