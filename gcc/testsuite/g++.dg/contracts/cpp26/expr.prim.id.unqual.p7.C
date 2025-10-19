// N5008 [expr.prim.id.unqual]/p7
// Otherwise, if the unqualified-id appears in the predicate of a contract assertion C (6.10) and the entity is
// (7.1) — a variable declared outside of C of object type T,
// (7.2) — a variable or template parameter declared outside of C of type “reference to T”, or
// (7.3) — a structured binding of type T whose corresponding variable is declared outside of C,
// then the type of the expression is const T
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts" }


static_assert (__cpp_contracts >= 202502L);

int gi = 7;
int &gri = gi;
int *gpi = &gi;

bool f(int&&){ return true;}
int temporary(){ return 4;}

void f1(int i) pre(i++); // { dg-error "increment of read-only location" }
void f2(int &i) pre(i++); // { dg-error "increment of read-only location" }
void f3(int *i) pre(i++); // { dg-error "increment of read-only location" }
void f4(int *i) pre((*i)++); // ok, not deep const
void f5(int *i) pre(gi++); // { dg-error "increment of read-only location" }
void f6() pre(f(temporary())); // ok, lifetime started within pre condition


struct S{

  int i;
  mutable int mi;
  int *pi = &i;
  int &ri = i;
  int &rmi = mi;
  int *pmi = &mi;


  void f(){
    int array[2] = {1,2};
    auto [sb1, sb2] = array;

    contract_assert(i++); // { dg-error "increment of member" }
    contract_assert(mi++); // ok, mutable
    contract_assert(sb1++); // { dg-error "increment of read-only location" }


    contract_assert(ri++); // ok, not deep const
    contract_assert(rmi++); // ok, not deep const

    contract_assert(pi++); // { dg-error "increment of member" }
    contract_assert((*pi)++); // ok, not deep const


    contract_assert(pmi++); // { dg-error "increment of member" }
    contract_assert((*pmi)++); // ok, not deep const

    contract_assert(gi++); // { dg-error "increment of read-only location" }
    contract_assert(gpi++); // { dg-error "increment of read-only location" }
    contract_assert((*gpi)++); // ok, not deep const
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

int n = 0;
struct X { bool m(); };

struct Y {
  int z = 0;

  void f(int i, int* p, int& r, X x, X* px)
    pre (++n)       // { dg-error "increment of read-only location" }
    pre (++i)       // { dg-error "increment of read-only location" }
    pre (++(*p))    // OK
    pre (++r)       // { dg-error "increment of read-only location" }
    pre (x.m())     // { dg-error "discards qualifiers" }
    pre (px->m());  // OK

  template <int N, int& R, int* P>
  void g()
    pre(++N)        // { dg-error "increment of read-only location" }
    // { dg-error {lvalue required as increment operand} "" { target *-*-* } .-1 }
    pre(++R)        // { dg-error "increment of read-only location" }
    pre(++(*P));    // OK

  int& k()
    post(r : ++r);  // { dg-error "increment of read-only location" }
};

int main()
{
  int i;
  contract_assert(i++); // { dg-error "increment of read-only location" }
  i = 5;

  int& ri = i;
  contract_assert(ri++); // { dg-error "increment of read-only location" }
  ri = 6;

  contract_assert(gi++); // { dg-error "increment of read-only location" }
  contract_assert(gri++); // { dg-error "increment of read-only location" }

  int& rgi = gi;
  contract_assert(rgi++); // { dg-error "increment of read-only location" }
  rgi = 6;


  int *pi= &i;
  contract_assert(pi++); // { dg-error "increment of read-only location" }
  contract_assert((*pi)++); // ok, not deep const

  contract_assert(i == 4 ? i : i ); // ok, no name clash


  return 0;
}
