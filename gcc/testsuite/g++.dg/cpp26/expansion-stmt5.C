// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++14 } }
// { dg-options "" }

void
foo (int x)
{
  switch (x)
    {
    case 1:
      template for (auto g : { 1, 2U, 3LL, 4ULL })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{						// { dg-message " enters 'template for' statement" "" { target *-*-* } .-1 }
	case 2:						// { dg-error "jump to case label" }
	  break;
	}
    case 3:
      template for (auto g : { 1, 2U, 3LL, 4ULL })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{						// { dg-message " enters 'template for' statement" "" { target *-*-* } .-1 }
	default:					// { dg-error "jump to case label" }
	  break;
	}
    }
  template for (auto g : { 1, 2U, 3LL, 4ULL })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      lab1:;						// { dg-error "identifier label 'lab1' in 'template for' body" }
    }
  switch (x)
    {
    case 1:
      template for (auto g : {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{						// { dg-message " enters 'template for' statement" "" { target *-*-* } .-1 }
	case 2:						// { dg-error "jump to case label" }
	  break;
	}
    case 3:
      template for (auto g : {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{						// { dg-message " enters 'template for' statement" "" { target *-*-* } .-1 }
	default:					// { dg-error "jump to case label" }
	  break;
	}
    }
  template for (auto g : {})				// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      lab2:;						// { dg-error "identifier label 'lab2' in 'template for' body" }
    }
}

template <typename T, T N>
void
bar (int x)
{
  switch (x)
    {
    case 1:
      template for (auto g : { 1, N, 3LL, 4ULL })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{						// { dg-message " enters 'template for' statement" "" { target *-*-* } .-1 }
	case 2:						// { dg-error "jump to case label" }
	  break;
	}
    case 3:
      template for (auto g : { 1, N, 3LL, 4ULL })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{						// { dg-message " enters 'template for' statement" "" { target *-*-* } .-1 }
	default:					// { dg-error "jump to case label" }
	  break;
	}
    }
  template for (auto g : { 1, 2U, N, 4ULL })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      lab1:;						// { dg-error "identifier label 'lab1' in 'template for' body" }
    }
  switch (x)
    {
    case 1:
      template for (auto g : {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{						// { dg-message " enters 'template for' statement" "" { target *-*-* } .-1 }
	case 2:						// { dg-error "jump to case label" }
	  break;
	}
    case 3:
      template for (auto g : {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
	{						// { dg-message " enters 'template for' statement" "" { target *-*-* } .-1 }
	default:					// { dg-error "jump to case label" }
	  break;
	}
    }
  template for (auto g : {})				// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      lab2:;						// { dg-error "identifier label 'lab2' in 'template for' body" }
    }
}

void
baz (int x)
{
  bar <unsigned, 2U> (x);
}
