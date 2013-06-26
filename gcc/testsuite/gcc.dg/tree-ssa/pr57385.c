/* { dg-do compile } */
/* { dg-options "-O1" } */

int c;

void foo(int f)
{
  int wbi=-100000000;
  c = (f ? "012346000000000000":"01345:000000006008")[wbi];
}
