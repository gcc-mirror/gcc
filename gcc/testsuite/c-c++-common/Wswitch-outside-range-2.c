// PR c++/90875
// { dg-options -Wno-switch-outside-range }

void f(char c)
{
  switch (c)
    case 300: //{ dg-bogus "case label value is less than minimum value for type" }
    case -300:; // { dg-bogus "case label value is less than minimum value for type" }
}
