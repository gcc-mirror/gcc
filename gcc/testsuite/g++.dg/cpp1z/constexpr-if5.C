// { dg-do compile { target c++11 } }
// { dg-options "-w" }

void f()
{
  if constexpr (false)		// { dg-message "enters constexpr if" }
    {
      goto l;			// { dg-message "from here" }
    }
  else
    {
    l:;				// { dg-error "jump to label" }
    }
}
