/* { dg-do compile } */
/* { dg-options "" } */

const char *
f1 (int flag)
{
  return flag ? "" : 1; /* { dg-error "pointer/integer type mismatch in conditional expression \\\[-Wint-conversion\\\]" } */
}

const char *
f2 (int flag)
{
  return flag ? 1 : ""; /* { dg-error "pointer/integer type mismatch in conditional expression \\\[-Wint-conversion\\\]" } */
}
