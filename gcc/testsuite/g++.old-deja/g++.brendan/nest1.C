// Build don't link: 
// GROUPS passed nested-classes
int x;
class enclose {
public:
  int x;

  class inner {
  public:
    void f (int i) {
      x = i;// ERROR - .*
    }
  };
};

