// PR c++/77337
// { dg-do compile { target c++14 } }

template<typename Functor>
struct fix_type {
  Functor functor;

  decltype(auto) operator()()
  { return functor(*this); }
};

template<typename Functor>
fix_type<Functor> fix(Functor functor)
{ return { functor }; }

int main()
{
  auto zero = fix
    ([](auto& self) -> int // N.B. non-deduced, non-dependent return type
     {
       return 0;

       self(); // { dg-bogus "use of \[^\n\r]* before deduction of 'auto'" }
     });

  return zero();
}
