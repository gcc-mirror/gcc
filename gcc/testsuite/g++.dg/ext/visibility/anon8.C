// PR c++/34213
// { dg-do compile }

template <void (*fn) ()>
void call ()			// { dg-message "note" }
{
  fn ();
}

namespace
{
  struct B1
  {
    static void fn1 () {}
    static void fn4 ();
  };
  void fn3 () {}
  void B1::fn4 () {}
  static void fn5 () {}
}

int main ()
{
  struct B2
  {
    static void fn2 () {}
  };
  call<&B1::fn1> ();
  call<&B2::fn2> ();	// { dg-error "not external linkage|no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 29 }
  call<&fn3> ();
  call<&B1::fn4> ();
  call<&fn5> ();	// { dg-error "not external linkage|no matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 33 }
}
