// PR c++/41769

void f(void operator+()); // { dg-error "13:declaration" }

void g()
{
  try
    {

    }
  catch(void operator+()) // { dg-error "14:declaration" }
    {
    }
}
