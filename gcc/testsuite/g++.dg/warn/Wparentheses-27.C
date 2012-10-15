// PR c++/54194
// { dg-options "-Wparentheses" }

int main()
{
  char in[4] = { 0 };
  in[1] = in[1] & 0x0F | ((in[3] & 0x3C) << 2); // { dg-warning "17:suggest parentheses" }
}
