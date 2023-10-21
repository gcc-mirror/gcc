/* { dg-additional-options "-std=gnu89" } */

typedef struct
{
  char x;
} s1;

s1 f (int arg0,...)
{
  int args;
  s1 back;
  va_start (args, arg0);
  va_end (args);
  return back;
}
