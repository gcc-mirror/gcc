// { dg-additional-options "-fmodules-ts" }
import One;

int vcall (base *ptr)
{
  return ptr->getter ();
}

int main ()
{
  base b (0xfeed);
  
  if (!(vcall (&b) == 0xfeed))
    return 1;

  return 0;
}
