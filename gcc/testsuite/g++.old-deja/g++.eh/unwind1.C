// Test that unwinding properly restores SP.
// Contributed by Jason Merrill <jason@cygnus.com>

void f (int i)
{
  throw i;
}

int main ()
{  
  void *sp1 = __builtin_alloca (0);

  try
    {
      f (0);
    }
  catch (int)
    {
    }

  void *sp2 = __builtin_alloca (0);

  return (sp1 != sp2);
}
