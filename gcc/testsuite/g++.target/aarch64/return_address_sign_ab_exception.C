/* { dg-do run } */
/* { dg-options "--save-temps" } */
/* { dg-require-effective-target arm_v8_3a_bkey_directive } */

__attribute__((target("branch-protection=pac-ret+leaf")))
int foo_a () {
  throw 22;
}

__attribute__((target("branch-protection=pac-ret+leaf+b-key")))
int foo_b () {
  throw 22;
}

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

/* { dg-final { scan-assembler-times "paciasp" 1 } } */
/* { dg-final { scan-assembler-times "pacibsp" 1 } } */
/* { dg-final { scan-assembler-times ".cfi_b_key_frame" 1 } } */

