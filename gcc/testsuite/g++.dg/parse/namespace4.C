/* PR c++/4652 */
/* { dg-do compile } */
/* Another conflict between namespace IDs and other things. */

namespace A { }

class B {

   struct {
      int x;
   } A;

};

int main() {
   B b;
   return 0;
}
