/* This used to abort due to incorrect loop iteration count computation.  */

/* { dg-do run } */
/* { dg-options "-O2" } */

int test (unsigned char *data) 
{
  unsigned char *top;
  unsigned char *bottom;
  unsigned int i = 0;

  for (bottom = data, top = data + 36;
       top > bottom;
       bottom++, top--)
    {
      i++;
    }

  return i;
}

int main (void)
{
  unsigned char buffer[36];

  if (test (buffer) != 18)
    abort ();

  exit (0);
}

