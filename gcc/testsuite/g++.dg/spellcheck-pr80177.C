// { dg-do compile { target c++11 } }

void pr80177 ()
{
  static_assertion (1 == 0, "1 == 0"); // { dg-error "3: 'static_assertion' was not declared in this scope" }
  // { dg-message "3: suggested alternative: 'static_assert'" "" { target *-*-* } .-1 }
}
