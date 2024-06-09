// PR c++/109172

// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

class Demo
{
  ~Demo();
};

int main()
{
  try
    {
      throw *new Demo;		// { dg-error private }
    }
  catch(const Demo& e) { }
}
