// { dg-do assemble  }

// submitted by David C Binderman <dcb@pncl.co.uk>

// According to [dcl.init]/9, this should be ill-formed

void
f()
{
  const int var [ 10 ]; // { dg-error "" } missing initializer 
}
