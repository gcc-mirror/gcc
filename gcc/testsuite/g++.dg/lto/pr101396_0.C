/* { dg-lto-do link } */

enum A : __UINT32_TYPE__ { // { dg-lto-warning "6: type 'A' violates the C\\+\\+ One Definition Rule" }
  a, // { dg-lto-note "3: name 'a' is defined as 32-bit while another translation unit defines it as 64-bit" }
  b,
  c
};

int main()
{
  return (int) A::a;
}
