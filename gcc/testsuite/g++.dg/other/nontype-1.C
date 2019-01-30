template <class Op>
bool asfun(Op f,
           Op::first_argument_type a, // { dg-error "not a type" }
           Op::second_argument_type b) // { dg-error "not a type" }
{
   return Op(a, b);
}
