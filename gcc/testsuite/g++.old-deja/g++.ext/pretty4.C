// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 3 Mar 2000 <nathan@codesourcery.com>

// __PRETTY_FUNCTION__, __FUNCTION__ and __function__ should have the
// type char const [X], where X is the right value for that particular function

static void const *strings[4];
static void const *tpls[4];
static unsigned pos = 0;
static int fail;
static void const *ptr = 0;

void unover (char const (*)[5]) {}
void foo (char const (*)[5]) {}
void foo (void *) {fail = 1;}
void foo (void const *) {fail = 1;}
void baz (char const (&)[5]) {}

template<unsigned I> void PV (char const (&objRef)[I])
{
  strings[pos] = objRef;
  tpls[pos] = __PRETTY_FUNCTION__;
  pos++;
}

void fn ()
{
  PV (__FUNCTION__);
  PV (__func__);
  PV (__PRETTY_FUNCTION__);
  PV ("wibble");
}

void baz ()
{
  ptr = __FUNCTION__;
  // there should be no string const merging
  if (ptr == "baz")
    fail = 1;
  // but all uses should be the same.
  if (ptr != __FUNCTION__)
    fail = 1;
}
int baz (int)
{
  return ptr == __FUNCTION__;
}

int main ()
{
  // make sure we actually emit the VAR_DECL when needed, and things have the
  // expected type.
  foo (&__FUNCTION__);
  baz (__FUNCTION__);
  unover (&__FUNCTION__);
  if (fail)
    return 1;
  
  // __FUNCTION__ should be unique across functions with the same base name
  // (it's a local static, _not_ a string).
  baz ();
  if (fail)
    return 1;
  if (baz (1))
    return 1;
  fn ();
  
  // Check the names of fn. They should all be distinct strings (though two
  // will have the same value).
  if (strings[0] == strings[1])
    return 1;
  if (strings[0] == strings[2])
    return 1;
  if (strings[1] == strings[2])
    return 1;

  // check the names of the template functions so invoked
  if (tpls[0] != tpls[1])
    return 1;
  if (tpls[0] == tpls[2])
    return 1;
  
  return 0;
}
