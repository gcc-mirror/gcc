// prms-id: 9028
class Foo;

int main()
{
  int i=0;
  switch (i) 
    {
    case ((Foo *)0): // ERROR - 
    case ((Foo *)1): // ERROR - 
      break;
    }
}
