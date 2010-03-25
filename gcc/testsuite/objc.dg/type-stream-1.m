/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */
#include <objc/typedstream.h>
#include <stdio.h>
#include <stdlib.h>

int main (void)
{
  FILE *f; TypedStream *ts;
  struct T { int a, b; } x = { 1, 2 };
  f = fopen ("foo", "w"); ts = objc_open_typed_stream (f, OBJC_WRITEONLY);
  objc_write_type (ts, @encode(struct T), &x);
  objc_close_typed_stream (ts); fclose (f);
  f = fopen ("foo", "r"); ts = objc_open_typed_stream (f, OBJC_READONLY);
  struct T y;
  objc_read_type (ts, @encode(struct T), &y);
  if (y.a != 1)
   abort ();
  if (y.b != 2)
   abort ();
  objc_close_typed_stream (ts); fclose (f);
  remove ("foo");
  return 0;
}

