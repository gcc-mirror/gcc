// { dg-do compile { target c++20 } }
// { dg-options "-w" }

void f()
{
  if consteval			// { dg-message "enters 'consteval if'" }
    {
      goto l;			// { dg-message "from here" }
    }
  else
    {
    l:;				// { dg-error "jump to label" }
    }
}
