// PR c++/39866
// { dg-options "-std=c++0x" }

struct A {
  A& operator=(const A&) = delete; // { dg-bogus "" }

  void operator=(int) {}	// { dg-message "" }
  void operator=(char) {}	// { dg-message "" }
};

struct B {};

int main()
{
  A a;
  a = B();		// { dg-error "no match" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 16 }
  a = 1.0;		// { dg-error "ambiguous" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 18 }
}
