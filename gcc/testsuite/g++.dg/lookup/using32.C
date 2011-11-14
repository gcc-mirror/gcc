// { dg-do compile }

struct T { struct type {}; };
struct T2 : T { using T::type; };
struct T3 : T2
{
  struct type {};
  type t;
};
