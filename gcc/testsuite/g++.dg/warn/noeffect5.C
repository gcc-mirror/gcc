// { dg-options "-Wall" }
// PR c++/15083

extern "C" int printf(const char*,...);
struct Counter {
  Counter(){printf("Hello World.\n");}
};
template< typename T >
void resetData() {
  new Counter();
}
int main() {
  resetData<int>();
} 
