// { dg-do assemble  }
// GROUPS passed pointers-to-members
class my_class 
{
public:
  typedef void func_type (int num);
  my_class (int num, func_type* proc);
  void dispatch (void);
private:
  int _num;
  func_type *_proc;
};

my_class::my_class (int num, func_type* proc) : _num(num), _proc(proc) 
{
}

void my_class::dispatch (void)
{
  _proc(_num);
}
