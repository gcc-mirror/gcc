/* { dg-do compile } */
/* { dg-options "-mtune=btver2 -mmemcpy-strategy=rep_8byte:-1:noalign" }

/* { dg-error "strategy name 'rep_8byte' specified for option '-mmemcpy_strategy=' not supported for 32-bit code" "" { target ia32 } 0 } */

struct U9
{
  unsigned a[9];
};

struct U9 u9;

void
foo ()
{
  u9 = (struct U9) {
    .a = {
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF
    }
  };
}
