// PR c++/57599

class A { };

class B : public A { };

void p()
{
  B* b;

  A* a1;
  a1 = dynamic_cast<A*>(b);
  a1 = dynamic_cast<const A*>(b);          // { dg-error "invalid" }
  a1 = dynamic_cast<volatile A*>(b);       // { dg-error "invalid" }
  a1 = dynamic_cast<const volatile A*>(b); // { dg-error "invalid" }

  const A* a2;
  a2 = dynamic_cast<A*>(b);
  a2 = dynamic_cast<const A*>(b);
  a2 = dynamic_cast<volatile A*>(b);       // { dg-error "invalid" }
  a2 = dynamic_cast<const volatile A*>(b); // { dg-error "invalid" }

  volatile A* a3;
  a3 = dynamic_cast<A*>(b);
  a3 = dynamic_cast<const A*>(b);          // { dg-error "invalid" }
  a3 = dynamic_cast<volatile A*>(b);
  a3 = dynamic_cast<const volatile A*>(b); // { dg-error "invalid" }

  const volatile A* a4;
  a4 = dynamic_cast<A*>(b);
  a4 = dynamic_cast<const A*>(b);
  a4 = dynamic_cast<volatile A*>(b);
  a4 = dynamic_cast<const volatile A*>(b);
}

void r()
{
  B b;

  A& a1 = dynamic_cast<A&>(b);
  A& a2 = dynamic_cast<const A&>(b);                // { dg-error "invalid" }
  A& a3 = dynamic_cast<volatile A&>(b);             // { dg-error "invalid" }
  A& a4 = dynamic_cast<const volatile A&>(b);       // { dg-error "invalid" }

  const A& ca1 = dynamic_cast<A&>(b);
  const A& ca2 = dynamic_cast<const A&>(b);
  const A& ca3 = dynamic_cast<volatile A&>(b);       // { dg-error "invalid" }
  const A& ca4 = dynamic_cast<const volatile A&>(b); // { dg-error "invalid" }

  volatile A& va1 = dynamic_cast<A&>(b);
  volatile A& va2 = dynamic_cast<const A&>(b);       // { dg-error "invalid" }
  volatile A& va3 = dynamic_cast<volatile A&>(b);
  volatile A& va4 = dynamic_cast<const volatile A&>(b);// { dg-error "invalid" }

  const volatile A& cva1 = dynamic_cast<A&>(b);
  const volatile A& cva2 = dynamic_cast<const A&>(b);
  const volatile A& cva3 = dynamic_cast<volatile A&>(b);
  const volatile A& cva4 = dynamic_cast<const volatile A&>(b);
}
