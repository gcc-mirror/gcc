// PR c++/97536
// { dg-do compile { target concepts } }

template<typename>
concept C1 = true;

concept C2 = true; // { dg-error "non-template variable cannot be .concept." }
// { dg-error "concept definition syntax is" "" { target *-*-* } .-1 }

template<typename>
void fn1 ()
{
  concept bar = true; // { dg-error "concept must be defined at namespace scope" }
// { dg-error "concept definition syntax is" "" { target *-*-* } .-1 }
}

void fn2 ()
{
  concept bar = true; // { dg-error "non-template variable cannot be .concept." }
// { dg-error "concept definition syntax is" "" { target *-*-* } .-1 }
}

template<typename>
void fn3 ()
{
  template<typename> // { dg-error "template declaration cannot appear at block scope" }
  concept bar = true;
}

void fn4 ()
{
  template<typename> // { dg-error "template declaration cannot appear at block scope" }
  concept bar = true;
}

void fn5 ()
{
  C1 auto x = 42;
}

template<typename>
void fn6 ()
{
  C1 auto x = 42;
}
