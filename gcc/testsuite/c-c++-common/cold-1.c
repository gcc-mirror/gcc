/* PR ipa/93087 */
/* { dg-do compile { target nonpic } } */
/* { dg-options "-O1 -Wsuggest-attribute=cold" } */

extern void *getdata (void);
extern int set_error (char const *message) __attribute__((cold));

__attribute__((cold)) int
set_nomem (void)	/* { dg-bogus "function might be candidate for attribute 'cold'" } */
{
  return set_error ("Allocation failed");
}

void *
getdata_or_set_error (void)
{
  void *result;
  result = getdata ();
  if (!result)
    set_nomem ();
  return result;
}
