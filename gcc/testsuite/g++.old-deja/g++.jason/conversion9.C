// PRMS Id: 8475

class SomeClass {
public:
  operator int & () {
    static int x;
    return x;
  }
} a;

int main (int, char**) {
  return a + 0;
}
