// { dg-do assemble  }

int i;

template <void (&FN)()>
void g ()
{
  FN ();
}

void h ()
{
  i = 7;
}

template void g<h>();
