/* Origin: <steven@gcc.gnu.org>
   Make sure we do not ICE when the type in the function
   argument list is incomplete (Bug 10602).  */
/* { dg-options "-w" } */

int g95_type_for_mode (enum machine_mode);

int
g95_type_for_mode (enum machine_mode mode)
{ /* { dg-error "has incomplete type" } */
  return 0;
}
