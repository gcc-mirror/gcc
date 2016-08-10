// { dg-do compile { target c++11 } }
// { dg-options "-w" }

void f()
{
  if constexpr (false)
    {
    l:;				// { dg-error "jump to label" }
    }
  else
    {
      goto l;			// { dg-message "from here" }
    }
}
