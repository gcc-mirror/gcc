// PR c++/70393
// { dg-do run { target c++11 } }

/* 'ab' has a static initializer, but we flubbed the initializer,
   because of B being the primary base.  */

struct A
{
  int a = 1;
};

struct B
{
  B *element = (B*)2;

    virtual int vfunc() = 0;

    int call_element()
    {
      return element->vfunc();
    }

    void set_element()
    {
      element = this;
    }
};

struct AB : public A, public B
{
    int vfunc()
    {
      return 0;
    }
};

static AB ab;

int main()
{
  if (ab.a != 1)
    return 1;
  if (ab.element != (void*)2)
    return 2;
  
  ab.set_element();
  return ab.call_element();
}

