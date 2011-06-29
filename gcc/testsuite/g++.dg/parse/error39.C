// PR c++/41769

void f(void operator+()); // { dg-error "declaration" }

void g()
{
  try
    {

    }
  catch(void operator+()) // { dg-error "declaration" }
    {
    }
}
