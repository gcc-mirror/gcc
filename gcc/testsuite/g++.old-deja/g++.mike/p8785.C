// Build don't link:
// prms-id: 8785

class Outer {
private:
  int x; // ERROR - private
public:
  struct Inner {
    int y;
    void f( Outer * p, int i) {
      p->x = i;			// ERROR - 
    };
    void f( Outer & p) {
      p.x = y;			// ERROR - 
    };
  };
};

int main() {
  Outer::Inner A;
  Outer Thing;

  A.f(Thing);
  A.f(&Thing,2);
}
