// PR c++/20905

struct name {};

int 
f ();

void 
g ()
{
  if (int name = f ())
    {
    }
}
