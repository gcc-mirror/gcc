/* We shouldn't crash trying to produce the inlined structure type die debug info.  */
/* { dg-do compile } */
/* { dg-options "-O1 -g" } */
inline void bar(char a[], unsigned int l)
{
  asm volatile ("" :: "m" ( *(struct {char x[l]; } *)a));
}

void foo(void)
{
  bar (0, 0);
}
