// Build don't link:

// submitted by David C Binderman <dcb@pncl.co.uk>

// According to [dcl.init]/9, this should be ill-formed

void
f()
{
  const int var [ 10 ]; // ERROR - missing initializer 
}
