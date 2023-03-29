// PR c++/109172

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
