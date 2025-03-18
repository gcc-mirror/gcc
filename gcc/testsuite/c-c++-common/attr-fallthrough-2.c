/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra -Wno-unused -Wno-implicit-fallthrough" } */

extern void bar (int);
void
fn (int i)
{
  switch (i)
  {
  case 1:
    bar (1);
    __attribute__((used));
    /* { dg-warning "empty declaration" "" { target c } .-1 } */
    /* { dg-warning "ignored" "" { target c++ } .-2 } */
  case 2:
    bar (1);
    __attribute__((foo));
    /* { dg-warning "empty declaration" "" { target c } .-1 } */
    /* { dg-warning "ignored" "" { target c++ } .-2 } */
  case 3:
    bar (1);
    __attribute__((fallthrough)) /* { dg-warning "not followed" "" { target c } } */
  case 4: /* { dg-error "expected" "" { target c } } */
    bar (1); /* { dg-warning "'fallthrough' attribute ignored" "" { target c++ } .-1 } */
    __attribute__((fallthrough)) 1;
    /* { dg-error "expected" "" { target c } .-1 } */
    /* { dg-warning "not followed" "" { target *-*-* } .-2 } */
  case 5:
    bar (1);
    __attribute__((fallthrough)) int i; /* { dg-warning "ignored|not followed" } */
  case 6:
    bar (1);
    __attribute__((fallthrough ("x"))); /* { dg-warning "specified with a parameter" } */
  case 7:
    bar (1);
    __attribute__((fallthrough, fallthrough)); /* { dg-warning "specified multiple times" } */
  case 8:
    bar (1);
    __attribute__((fallthrough));
  case 9:
    __attribute__((fallthrough));
    /* { dg-warning "not preceding" "" { target *-*-* } .-1 } */
    bar (1);
  case 10:
    bar (1);
    __attribute__((unused, fallthrough)); /* { dg-warning "attribute ignored" } */
  case 11:
    bar (1);
    __attribute__((fallthrough, unused)); /* { dg-warning "attribute ignored" } */
  default:
    bar (99);
  }
}
