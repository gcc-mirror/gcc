/* { dg-do run } */
/* { dg-options "-mbranch-protection=pac-ret+leaf+b-key --save-temps" } */
/* { dg-require-effective-target arm_v8_3a_bkey_directive } */

int foo () {
  throw 22;
}

int main (int argc, char** argv) {
  try {
    foo();
  } catch (...) {
    return 0;
  }
  return 1;
}

/* { dg-final { scan-assembler-times ".cfi_b_key_frame" 2 } } */
