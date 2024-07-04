// generic assert contract parsing checks
//   check omitted, 'default', 'audit', and 'axiom' contract levels parse
//   check that all concrete semantics parse
//   check omitted, '%default' contract roles parse
//   ensure that an invalid contract level 'invalid' errors
//   ensure that a predicate referencing an undefined variable errors
//   ensure that a missing colon after contract level errors
//   ensure that an invalid contract role 'invalid' errors
//   ensure that a missing colon after contract role errors
// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr " }
// { dg-do compile }


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

template <class T> void perfect_forward(T&& t) pre(++t) {} // { dg-error "increment of read-only" }

struct S2
{
  int i = 0;
  template <class Self> void perfect_forward(this Self&& self) pre(++self.i) {} // { dg-error "increment of member.*in read-only object" }
};

void template_related_tests()
{
  int i = 0;
  const int ci = 42;
  perfect_forward(i);
  perfect_forward(666);
  perfect_forward(ci);
  perfect_forward((const int&&) ci);
  S2 s;
  const S2 cs;
  s.perfect_forward();
  cs.perfect_forward();
  S2().perfect_forward();
  ((const S2&&)S2()).perfect_forward();
}

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
