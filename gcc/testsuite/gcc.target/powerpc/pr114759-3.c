/* PR target/114759 */
/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -mrop-protect" } */

/* Verify we emit an error if we use -mrop-protect with an unsupported cpu
   or ABI.  */

extern void foo (void);

int
bar (void)
{
  foo ();
  return 5;
}

/* The correct line number is in the preamble to the error message, not
   in the final line (which is all that dg-error inspects). Hence, we have
   to tell dg-error to ignore the line number.  */
/* { dg-error "'-mrop-protect' requires '-mcpu=power8'" "PR114759" { target *-*-* } 0 } */
/* { dg-error "'-mrop-protect' requires the ELFv2 ABI" "PR114759" { target { ! rop_ok } } 0 } */
