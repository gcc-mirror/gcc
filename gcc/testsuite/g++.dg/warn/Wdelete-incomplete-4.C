// PR c++/63619
// { dg-options -Wno-delete-incomplete }

int main() {
   void* p;
   delete p;
}
