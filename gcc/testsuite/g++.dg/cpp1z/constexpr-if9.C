// { dg-do compile { target c++11 } }
// { dg-options "-w" }

void f(int i)
{
  switch (i)
    if constexpr (false)	// { dg-message "enters constexpr if" }
      {
      case 42:;			// { dg-error "jump to case label" }
      }
}
