// { dg-do compile { target c++20 } }
// { dg-options "-w" }

void f()
{
  if consteval
    {
    l:;				// { dg-error "jump to label" }
    }
  else
    {
      goto l;			// { dg-message "from here" }
    }
}
