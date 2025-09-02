// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++23 } }

void
g ()
{
  /* It looks like this shouldn't work but [expr.prim.lambda.closure]/6
     says "Otherwise, it is a non-static member function or member function
     template that is declared const if and only if the lambda-expression's
     parameter-declaration-clause is not followed by mutable and the
     lambda-declarator does not contain an explicit object parameter."  */
  auto counter = [j=0](this auto const& self) -> decltype(j) {
      return j++;
  };

  auto counter2 = [j=0](this auto& self) -> decltype(j) {
      return j++;
  };
}
