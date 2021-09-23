// { dg-do compile { target c++20 } }
// { dg-options "-w" }

constexpr void f(int i)
{
  switch (i)
    if consteval		// { dg-message "enters 'consteval if'" }
      {
      case 42:;			// { dg-error "jump to case label" }
      }
}
