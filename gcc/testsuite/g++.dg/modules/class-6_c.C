// { dg-additional-options "-fmodules-ts" }
import One;

int vcall (derived *ptr)
{
  return ptr->getter ();
}

int main ()
{
  derived b (0xfeed);
  
  if (!(vcall (&b) == 0xfeed))
    return 1;

  return 0;
}
