// generic assert contract parsing checks
//   check omitted, 'default', 'audit', and 'axiom' contract levels parse
//   check that all concrete semantics parse
//   check omitted, '%default' contract roles parse
//   ensure that an invalid contract level 'invalid' errors
//   ensure that a predicate referencing an undefined variable errors
//   ensure that a missing colon after contract level errors
//   ensure that an invalid contract role 'invalid' errors
//   ensure that a missing colon after contract role errors
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr " }

static_assert (__cpp_contracts >= 201906);
static_assert (__cpp_contracts_literal_semantics >= 201906);
static_assert (__cpp_contracts_roles >= 201906);

int gi = 7;
int &gri = gi;


void f1(int i) pre(i++); // { dg-error "increment of read-only location" }
void f2(int &i) pre(i++); // { dg-error "increment of read-only location" }
void f3(int *i) pre(i++); // { dg-error "increment of read-only location" }
void f4(int *i) pre((*i)++); // ok, not deep const
void f5(int *i) pre(gi++); //  ok, non automatic storage

// todo structured binding test
// lambda tests
// template tests

struct S{

  int i;
  mutable int mi;
  int *pi = &i;
  int &ri = i;
  int &rmi = mi;
  int *pmi = &mi;

  void f(){
    contract_assert(i++); // { dg-error "increment of member" }
    contract_assert(mi++); // ok, mutable

    contract_assert(ri++); // ok, not deep const
    contract_assert(rmi++); // ok, not deep const

    contract_assert(pi++); // { dg-error "increment of member" }
    contract_assert((*pi)++); // ok, not deep const


    contract_assert(pmi++); // { dg-error "increment of member" }
    contract_assert((*pmi)++); // ok, not deep const
  }

};

void class_related_tests()
{




}

int main()
{
  int i;
  contract_assert(i++); // { dg-error "increment of read-only location" }
  i = 5;

  int& ri = i;
  contract_assert(ri++); // { dg-error "increment of read-only location" }
  ri = 6;

  contract_assert(gi++); // ok, not automatic storage
  contract_assert(gri++); // ok, not automatic storage

  int& rgi = gi;
  contract_assert(rgi++); // { dg-error "increment of read-only location" }
  rgi = 6;


  int *pi= &i;
  contract_assert(pi++); // { dg-error "increment of read-only location" }
  contract_assert((*pi)++); // ok, not deep const

  contract_assert(i == 4 ? i : i ); // ok, no name clash


  return 0;
}
