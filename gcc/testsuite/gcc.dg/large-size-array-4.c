/* PR c/25309 */
/* { dg-do compile { target ilp32 } } */
/* { dg-options "" } */
static char * name[] = {
    [0x80000000]  = "bar"
  };
/* { dg-error "too large" "" { target { { ! lp64 } && { ! llp64 } } }  6 } */
