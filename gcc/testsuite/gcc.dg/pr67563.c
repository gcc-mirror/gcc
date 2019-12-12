/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions" } */
/* { dg-require-effective-target exceptions } */

static void
emit_package (int p1)
{
  int a;
  int b[0];
  a = __fprintf_chk (0, 0, "");
}
void emit_lua () { emit_package (0); }
