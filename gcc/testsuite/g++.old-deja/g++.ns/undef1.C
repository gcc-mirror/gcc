// { dg-do assemble  }
namespace A{
}

struct Y: A::S<int>{};     //{ dg-error "" } no such type
