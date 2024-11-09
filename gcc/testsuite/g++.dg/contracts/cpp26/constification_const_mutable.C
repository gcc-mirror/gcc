// generic assert contract parsing checks
//   check omitted, 'default', 'audit', and 'axiom' contract levels parse
//   check that all concrete semantics parse
//   check omitted, '%default' contract roles parse
//   ensure that an invalid contract level 'invalid' errors
//   ensure that a predicate referencing an undefined variable errors
//   ensure that a missing colon after contract level errors
//   ensure that an invalid contract role 'invalid' errors
//   ensure that a missing colon after contract role errors
// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr -fcontracts-nonattr-mutable-keyword -fcontracts-nonattr-const-keyword" }
// { dg-do compile }


static_assert (__cpp_contracts >= 201906);
static_assert (__cpp_contracts_literal_semantics >= 201906);
static_assert (__cpp_contracts_roles >= 201906);

int gi = 7;
int &gri = gi;


void f1(int i) pre const(i++); // { dg-error "increment of read-only location" }
void f2(int &i) pre const(i++); // { dg-error "increment of read-only location" }
void f3(int *i) pre const(i++); // { dg-error "increment of read-only location" }
void f4(int *i) pre const((*i)++); // ok, not deep const
void f5(int *i) pre const(gi++); //  ok, non automatic storage
void f6(int *i) pre const mutable(gi++); // { dg-error {cannot be both 'const' and 'mutable'} }
void f7(int *i) pre const((*i)++) pre mutable((*i)++); // ok, not deep const
void f8(int *i) pre const(gi++) pre mutable(gi++); //  ok, non automatic storage

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
    contract_assert const(i++); // { dg-error "increment of member" }
    contract_assert const(mi++); // ok, mutable
    contract_assert mutable const(i++); // { dg-error {cannot be both 'const' and 'mutable'} }
 // { dg-error {increment of member 'S::i' in read-only object} "" { target *-*-* } .-1 }
    contract_assert mutable const(mi++); // { dg-error {cannot be both 'const' and 'mutable'} }

    contract_assert const(ri++); // ok, not deep const
    contract_assert const(rmi++); // ok, not deep const

    contract_assert const(pi++); // { dg-error "increment of member" }
    contract_assert const((*pi)++); // ok, not deep const


    contract_assert const(pmi++); // { dg-error "increment of member" }
    contract_assert const((*pmi)++); // ok, not deep const
  }

};

template <class T> void perfect_forward(T&& t) pre const(++t) {} // { dg-error "increment of read-only" }

template <class T> void perfect_forward2(T&& t) pre mutable const(++t) {} // { dg-error {cannot be both 'const' and 'mutable'} }

struct S2
{
  int i = 0;
  template <class Self> void perfect_forward(this Self&& self) pre const(++self.i) {} // { dg-error "increment of member.*in read-only object" }
  template <class Self> void perfect_forward2(this Self&& self) pre const mutable(++self.i) {} // { dg-error {cannot be both 'const' and 'mutable'} }
};

void template_related_tests()
{
  int i = 0;
  const int ci = 42;
  perfect_forward(i);
  perfect_forward(666);
  perfect_forward(ci);
  perfect_forward((const int&&) ci);
  perfect_forward2(666);
// { dg-error {increment of read-only location} "" { target *-*-* } 66 }
  S2 s;
  const S2 cs;
  s.perfect_forward();
  s.perfect_forward2();
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
  contract_assert const(i++); // { dg-error "increment of read-only location" }
  i = 5;

  int& ri = i;
  contract_assert const(ri++); // { dg-error "increment of read-only location" }
  ri = 6;

  contract_assert const(gi++); // ok, not automatic storage
  contract_assert const(gri++); // ok, not automatic storage

  int& rgi = gi;
  contract_assert const(rgi++); // { dg-error "increment of read-only location" }
  rgi = 6;


  int *pi= &i;
  contract_assert const(pi++); // { dg-error "increment of read-only location" }
  contract_assert const((*pi)++); // ok, not deep const

  contract_assert const(i == 4 ? i : i ); // ok, no name clash


  return 0;
}
