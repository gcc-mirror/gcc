// { dg-do link  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void f (T&) ;

template <>
void f (void (&)()) 
{
}

void g () 
{
}

void h ()
{
}

bool b;

int main ()
{
  f (b ? g : h);
}

