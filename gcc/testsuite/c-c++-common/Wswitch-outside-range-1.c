// PR c++/90875

void f(char c)
{
  switch (c)
    case 300: // { dg-warning "case label value exceeds maximum value for type" }
    case -300:; // { dg-warning "case label value is less than minimum value for type" }
}
