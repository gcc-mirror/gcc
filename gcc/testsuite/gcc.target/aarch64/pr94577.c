/* { dg-do compile } */
/* { dg-options "-mcmodel=large -mabi=ilp32 -fno-pie" } */

void
foo ()
{
  // Do nothing
}

/* { dg-message "sorry, unimplemented: code model 'large' not supported in ilp32 mode"  "" { target *-*-* } 0 } */
