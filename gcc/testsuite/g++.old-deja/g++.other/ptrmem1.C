// { dg-do assemble  }

class x
{
public:
  virtual int is_constant();
};

void foo()
{
  x* y;
  if (y->is_constant) // { dg-error "" } assuming &
    {
    }
}
