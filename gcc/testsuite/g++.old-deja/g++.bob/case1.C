// { dg-do assemble  }
// prms-id: 9028
class Foo;

int main()
{
  int i=0;
  switch (i) 
    {
    case ((Foo *)0): // { dg-error "" } 
    case ((Foo *)1): // { dg-error "" } 
      break;
    }
}
