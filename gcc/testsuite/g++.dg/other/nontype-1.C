template <class Op>
bool asfun(Op f,
           Op::first_argument_type a, // { dg-error "not a type" "" { target c++17_down } }
           Op::second_argument_type b) // { dg-error "not a type" "" { target c++17_down } }
{
   return Op(a, b);
}
