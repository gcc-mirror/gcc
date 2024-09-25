// PR c++/116113
// { dg-do compile { target c++11 } }
// { dg-options "" }

extern int b[];

void
foo ()
{
  auto [a] = b;	// { dg-error "is incomplete" }
		// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  if (a)
    ;
  switch (a)
    {
    default:
      break;
    }
}
