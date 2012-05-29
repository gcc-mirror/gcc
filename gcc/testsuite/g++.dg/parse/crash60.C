// PR c++/53491

struct M
{
  void pop();
};

void foo()
{
  int result = 0;
  M m;

  result += m.pop();  // { dg-error "invalid operands|in evaluation" }
}
