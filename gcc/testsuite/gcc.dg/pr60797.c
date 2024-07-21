/* If there's support for symbol aliases, have to 'dg-skip-if' -- unless
   there's a way to disable this support.
   { dg-additional-options -mno-alias { target nvptx-*-* } }
   { dg-skip-if "" { { ! nvptx-*-* } && alias } } */

extern int foo __attribute__((alias("bar"))); /* { dg-error "supported" } */
int main()
{
  return 0;
}
