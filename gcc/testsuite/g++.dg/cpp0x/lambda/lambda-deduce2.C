// PR c++/43875
// { dg-options "-std=c++0x" }

int main()
{
   auto x2 = []{ return { 1, 2 }; }; // { dg-message "return" }
}
