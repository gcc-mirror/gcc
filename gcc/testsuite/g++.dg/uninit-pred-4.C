/* { dg-do compile } */
/* { dg-options "-Wuninitialized -Og" } */

int pop ();
int pop_first_bucket;

int my_pop ()
{
  int out;  // { dg-bogus "uninitialized" "uninitialized variable warning" }

  while (pop_first_bucket)
    if (pop_first_bucket && (out = pop()))
      return out;

  return 0;
}
