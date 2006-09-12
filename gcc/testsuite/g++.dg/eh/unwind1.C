// PR middle-end/28493

extern "C" void abort ();

struct Command *ptr;

struct Command {
  Command() { ptr = this; }
  virtual ~Command() { if (ptr != this) abort(); }
};

void tryfunc()
{
  Command cmd;
  throw 1;
}

int main()
{
  try
    {
      tryfunc();
    }
  catch (int) { }
}
