// Special g++ Options: -fexceptions
// Build don't link:

class test1 {
public:
  class fehler{public:fehler(){};};
  void func(int a) {
    if( a == 0 )
      throw fehler();
  }
};

int main() {
  test1 var;

  try {
    var.func(1);
    var.func(0);
  } catch(test1::fehler())		// function type promoted to pointer
    {
      ;
    }
}
