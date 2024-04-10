/* { dg-do compile } */

struct {
  __INT64_TYPE__ m : 60;
} s;

short a;
short b;

void
foo ()
{
  s.m += a * b;
}
