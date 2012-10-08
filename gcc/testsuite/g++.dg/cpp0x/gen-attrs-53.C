// { dg-do compile { target c++11 } }

int
toto ()
{
  [[gnu::unused]] good:
    return 0;
}

int
foo ()
{
  [[gnu::unused]] good:
    int i = 0;

  // A C++11 attribute at the beginning of the return statement is
  // syntactically correct, appertains to the return statement (not to
  // the label) but is currently ignored by this implementation.
 good_ignored : [[gnu::unused]] // { dg-warning "attributes at the beginning of statement are ignored" }
    return i;
}

int
bar ()
{
  // A GNU attribute after the label appertains to the label.
 good: __attribute__((unused));
  return 0;
}

int
baz ()
{
  // The c++ attribute after the label appertains to the (empty)
  // statement.
 bad: [[gnu::unused]]; // { dg-warning "attributes at the beginning of statement are ignored" }
  return 0;
}

