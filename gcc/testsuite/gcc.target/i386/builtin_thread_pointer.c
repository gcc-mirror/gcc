/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-mtls-direct-seg-refs -O2 -masm=att" } */

int*
foo1 ()
{
  return (int*) __builtin_thread_pointer ();
}

/* { dg-final { scan-assembler "mov\[lq\]\[ \t\]*%\[fg\]s:0, %\[re\]ax" } }  */

int
foo2 ()
{
  int* p =  (int*) __builtin_thread_pointer ();
  return p[4];
}

/* { dg-final { scan-assembler "movl\[ \t\]*%\[fg\]s:16, %eax" } }  */

int
foo3 (int i)
{
  int* p = (int*) __builtin_thread_pointer ();
  return p[i];
}

/* { dg-final { scan-assembler "movl\[ \t\]*%\[fg\]s:0\\(,%\[a-z0-9\]*,4\\), %eax" { target { ! x32 } } } }  */
/* { dg-final { scan-assembler-not "movl\[ \t\]*%fs:0\\(,%\[a-z0-9\]*,4\\), %eax" { target x32 } } }  */
/* { dg-final { scan-assembler "movl\[ \t\]*\\(%eax,%edi,4\\), %eax" { target x32 } } }  */
