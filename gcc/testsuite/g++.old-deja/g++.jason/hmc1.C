// { dg-do assemble  }
// GROUPS passed templates default-arguments
template <class I>
class Klasse {
public:
  void func1(int n=1);
  void func2(int d) {}
};
template <class I> 
void Klasse<I>::func1(int n) {}

//if this is replaced by:
//void Klasse<I>::func1(int n=1) {}
//the code compiles.

int main() {
  Klasse<int> C;
  return 0;
}
