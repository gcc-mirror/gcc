// PR c/7652
// { dg-do compile { target c++11 } }
// { dg-options "-Wextra -Wall -Wpedantic" }

extern void bar (int);
void
fn (int i)
{
  [[gnu::fallthrough]] int j = 0; // { dg-warning "attribute ignored" }

  if (j)
    [[gnu::fallthrough]];  // { dg-error "invalid use" }

  [[gnu::fallthrough]];  // { dg-error "invalid use" }
  switch (i)
  {
    [[gnu::fallthrough]]; // { dg-warning "statement will never" }
  case 1:
   i++;
   [[gnu::fallthrough]];
  case 2:
    if (i) // { dg-warning "statement may fall through" }
      bar (2);
    else
      [[gnu::fallthrough]];
  case 3:
    if (i > 1)
      [[gnu::fallthrough]];
    else
      return;
  case 4:
    if (i)
      [[gnu::fallthrough]]; // { dg-warning "not preceding" }
    [[gnu::fallthrough]];
  case 5:
   ;
   [[gnu::fallthrough]];
  case 6:
    if (i) // { dg-warning "statement may fall through" }
      bar (6);
    else
      {
	[[gnu::fallthrough]];
      }
  case 7:
    if (i > 1)
      {
	[[gnu::fallthrough]];
      }
    else
      bar (7); // { dg-warning "statement may fall through" }
  default:
    --j;
  }

  [[gnu::fallthrough]]; // { dg-error "invalid use" }
}
