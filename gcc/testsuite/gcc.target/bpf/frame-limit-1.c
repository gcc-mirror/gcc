/* { dg-do compile } */
/* { dg-options "-O0" } */

/* The stack frame size is limited to 32767 bytes.  */

int
foo ()
{
  long data[4095];
  return 0;
}

int
bar ()
{
  long data[4096];
  return 0;
} /* { dg-error "stack limit" } */
