// Build don't link: 
// GROUPS passed templates default-arguments
template <class I>
class Class {
public:
  void func1(int n=1);
  void func2(int d) {}
};
template <class I> 
void Class<I>::func1(int n) {}

//if this is replaced by:
//void Class<I>::func1(int n=1) {}
//the code compiles.

int main() {
  Class<int> C;
  return 0;
}
