enum {e0, e1};

int x[] =
{
  [e0] = 0
};

f ()
{
  switch (1)
    {
    case e0:
    case e1:
      break;
    }
}
