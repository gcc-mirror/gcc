/* { dg-do compile } */
/* { dg-options "-Wconversion -Wsign-conversion" } */

void foo(int haveBar, char bar_)
{
  char zuul = haveBar?bar_:0;
  char zuul2 = haveBar?bar_:bar_;
  char zuul3 = haveBar?0:bar_;
}
