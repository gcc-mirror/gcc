// { dg-do assemble  }
struct A
{
  A();
  A(A&);			// { dg-message "A::A|no known conversion" "" { target c++14_down } } referenced below
};

int
main ()
{
  try
    {
      throw A();		// { dg-error "rvalue" "" { target c++14_down } } can't copy
      // { dg-message "13:thrown expression" "expr" { target c++14_down } .-1 }
    }
  catch (...) { }
}
