// generic assertion-statement parsing checks
// N5008
// assertion-statement :
// contract_assert attribute-specifier-seq opt ( conditional-expression ) ;
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts" }

static_assert (__cpp_contracts >= 202502L);

int main()
{
  int x;

  contract_assert( x >= 0);
  contract_assert( x < 0);
  contract_assert( x == 0);

  contract_assert( x > 0 ? true : false);
  contract_assert( x < 0 ? true : false);

  contract_assert( x >= 0);
  contract_assert( x >= 0);
  contract_assert( x >= 0);
  contract_assert( x >= 0);

  contract_assert( x >= 0);
  contract_assert( x < 0);
  contract_assert( x == 0);
  contract_assert( x == 1);



  return 0;
}
