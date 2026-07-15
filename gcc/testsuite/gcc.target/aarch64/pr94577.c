/* { dg-do compile } */
/* { dg-require-effective-target aarch64_mabi_ilp32 } */
/* { dg-options "-mcmodel=large -mabi=ilp32 -Wno-deprecated -fno-pie" } */

void
foo ()
{
  // Do nothing
}

/* { dg-message "sorry, unimplemented: code model 'large' not supported in ilp32 mode"  "" { target *-*-* } 0 } */
