// { dg-do run  }
// Origin: Mark Mitchell <mark@codesourcery.com>

int main() 
{
  typedef double I;
 
  struct S1 {
    typedef char I;
    
    struct S2;
  };
  
  struct S1::S2 {
    typedef I J;
  };
 
  return !(sizeof (S1::S2::J) == 1);
}
