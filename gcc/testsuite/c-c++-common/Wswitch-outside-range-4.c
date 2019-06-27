// PR c++/90875
// { dg-options "-Wno-pedantic -Wno-switch-outside-range" }

void f(char c)
{
  switch (c)
  
    case -300 ... 300:; // { dg-bogus "lower value in case label range less than minimum value for type|upper value in case label range exceeds maximum value for type" }
}
