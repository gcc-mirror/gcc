// Build don't link: 
// GROUPS passed overloading
class foo_int
{
public:
  int & i;

  foo_int (int &j) : i(j) {};
  void inc () { i++; }

};
