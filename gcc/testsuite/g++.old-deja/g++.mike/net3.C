// Build don't link:
// Here is another program from the net.

class BOOL {
 public:
  int val;
  
  BOOL(int i =0);
  operator int();
};

BOOL& foo()
{
  static BOOL status;
  return status;
}
