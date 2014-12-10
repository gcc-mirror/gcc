// PR c++/63619

int main() {
   void* p;
   delete p;    // { dg-warning "undefined" }
}
