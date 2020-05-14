// { dg-do compile { target c++20 } }
// { dg-options "-fext-numeric-literals" }

int main()
{
  // GCC complex literal extension  
  { constexpr auto v = 1 <=> 1i; } // { dg-error "invalid operands" }
  { constexpr auto v = 1i <=> 1.0i; } // { dg-error "invalid operands" }
}
