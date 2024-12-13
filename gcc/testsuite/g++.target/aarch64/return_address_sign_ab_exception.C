/* { dg-do run } */
/* { dg-options "--save-temps" } */
/* { dg-require-effective-target arm_v8_3a_bkey_directive } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** _Z5foo_av:
** 	hint	25 // paciasp
** ...
*/
__attribute__((target("branch-protection=pac-ret+leaf")))
int foo_a () {
  throw 22;
}

/*
** _Z5foo_bv:
** 	hint	27 // pacibsp
** ...
*/
__attribute__((target("branch-protection=pac-ret+leaf+b-key")))
int foo_b () {
  throw 22;
}
/* { dg-final { scan-assembler-times ".cfi_b_key_frame" 1 } } */

int main (int argc, char** argv) {
  try {
    foo_a ();
  } catch (...) {
    try {
      foo_b ();
    } catch (...) {
      return 0;
    }
  }
  return 1;
}
