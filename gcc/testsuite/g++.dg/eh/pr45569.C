// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions" }

float f ();
_Complex float g ();

void
i (_Complex float);

void j ()
{
  _Complex float x = 0;
  try
    {
      x = f ();
    }
  catch ( ...)
    {
      x += g ();
    }
  i (x);
}

