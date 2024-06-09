/* Test C23 attribute syntax: use of __extension__ in C11 mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#define FOO ::
#define BAR :
#define JOIN(A, B) A/**/B
#define JOIN2(A, B) A##B

typedef int [[__extension__ gnu::vector_size (4)]] g1;
typedef int [[__extension__ gnu :: vector_size (4)]] g2;
typedef int [[__extension__ gnu FOO vector_size (4)]] g3;
typedef int [[__extension__ gnu :: vector_size (sizeof (void (*)(...)))]] g4;
typedef int [[__extension__]] g5;
typedef int [[__extension__,]] g6;
typedef int [[__extension__, ,,,, ,, ,]] g7;
[[__extension__ deprecated]] int g8 ();
[[__extension__ nodiscard]] int g9 ();
[[__extension__ noreturn]] void g10 ();

int
cases (int x)
{
  switch (x)
    {
    case 1:
    case 2:
    case 4:
      x += 1;
      [[__extension__ fallthrough]];
    case 19:
    case 33:
      x *= 2;
      [[fallthrough]];  /* { dg-error {attributes before C23} } */
    case 99:
      return x;

    default:
      return 0;
    }
}

typedef int [[__extension__ vector_size (4)]] b1; /* { dg-error {'vector_size' attribute ignored} } */
typedef int [[__extension__ __extension__]] b2; /* { dg-error {'extension' attribute ignored} } */
typedef int [[__extension__ unknown_attribute]] b3; /* { dg-error {'unknown_attribute' attribute ignored} } */
typedef int [[__extension__ gnu:vector_size(4)]] b4; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
typedef int [[__extension__ gnu JOIN2(:,:) vector_size (4)]] b5; /* { dg-error {pasting ":" and ":" does not give a valid preprocessing token} } */
/* { dg-error {expected '\]' before ':'} "" { target *-*-* } .-1 } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-2 } */
typedef int [[__extension__ gnu : : vector_size (4)]] b6; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
typedef int [[__extension__ gnu: :vector_size (4)]] b7; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
typedef int [[__extension__ gnu BAR BAR vector_size (4)]] b8; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
typedef int [[__extension__ gnu :/**/: vector_size (4)]] b9; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
typedef int [[__extension__ gnu JOIN(:,:) vector_size (4)]] b10; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
typedef int [[gnu::vector_size(4)]] b11; /* { dg-error {attributes before C23} } */
typedef int [[gnu : : vector_size(4)]] b12; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
/* { dg-error {attributes before C23} "" { target *-*-* } .-2 } */
typedef int [[gnu : vector_size(4)]] b13; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
/* { dg-error {attributes before C23} "" { target *-*-* } .-2 } */
typedef int [[gnu: :vector_size (4)]] b14; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
/* { dg-error {attributes before C23} "" { target *-*-* } .-2 } */
typedef int [[gnu BAR BAR vector_size (4)]] b15; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
/* { dg-error {attributes before C23} "" { target *-*-* } .-2 } */
typedef int [[gnu :/**/: vector_size (4)]] b16; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
/* { dg-error {attributes before C23} "" { target *-*-* } .-2 } */
typedef int [[gnu JOIN(:,:) vector_size (4)]] b17; /* { dg-error {expected '\]' before ':'} } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-1 } */
/* { dg-error {attributes before C23} "" { target *-*-* } .-2 } */
typedef int [[gnu :: vector_size (4)]] b18; /* { dg-error {attributes before C23} } */
typedef int [[gnu FOO vector_size (4)]] b19; /* { dg-error {attributes before C23} } */
typedef int [[gnu :: vector_size (sizeof (void (*)(...)))]] b20; /* { dg-error {attributes before C23} } */
/* { dg-error {requires a named argument before} "" { target *-*-* } .-1 } */
typedef int [[gnu JOIN2(:,:) vector_size (4)]] b21; /* { dg-error {pasting ":" and ":" does not give a valid preprocessing token} } */
/* { dg-error {expected '\]' before ':'} "" { target *-*-* } .-1 } */
/* { dg-error {'gnu' attribute ignored} "" { target *-*-* } .-2 } */
/* { dg-error {attributes before C23} "" { target *-*-* } .-3 } */
