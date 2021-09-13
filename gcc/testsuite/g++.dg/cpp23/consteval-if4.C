// { dg-do compile { target c++20 } }
// { dg-options "-w" }

void f()
{
  goto l;			// { dg-message "from here" }
  if consteval			// { dg-message "enters 'consteval if'" }
    {
    l:;				// { dg-error "jump to label" }
    }
}

void g()
{
  goto l;			// { dg-message "from here" }
  if not consteval		// { dg-message "enters 'consteval if'" }
    {
    l:;				// { dg-error "jump to label" }
    }
}

void h()
{
  goto l;			// { dg-message "from here" }
  if consteval			// { dg-message "enters 'consteval if'" }
    {
    }
  else
    {
    l:;				// { dg-error "jump to label" }
    }
}

void i()
{
  goto l;			// { dg-message "from here" }
  if not consteval		// { dg-message "enters 'consteval if'" }
    {
    }
  else
    {
    l:;				// { dg-error "jump to label" }
    }
}
