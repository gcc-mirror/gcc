// { dg-do compile }
// { dg-options "-std=c++2a" }

int main()
{
  U'\U00110000'; // { dg-warning "outside" "110000 outside UCS" }
}
