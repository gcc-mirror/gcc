// PR c++/43452
// { dg-options -Wno-delete-incomplete }

class Foo;
int main() {
   Foo* p;
   delete [] p;
}
