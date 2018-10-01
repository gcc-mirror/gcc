// PR c++/51927
// { dg-do compile { target c++11 } }

struct function
{
  template<typename Functor>
  function(Functor);
};

struct testee
{
  function l1 = []() { };
  function l2 = [=]() { l1; }; // { dg-warning "implicit capture" "" { target c++2a } }
};
