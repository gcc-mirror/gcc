// { dg-do run { target { { i?86-*-* x86_64-*-* } && ia32 } } }

#define ATTR0 __attribute__((__regparm__(0)))
#define ATTR1 __attribute__((__regparm__(1)))
#define ATTR2 __attribute__((__regparm__(2)))
#define ATTR3 __attribute__((__regparm__(3)))
#define ATTR4 __attribute__((__fastcall__))
#define ATTR5 __attribute__((__stdcall__))
#define ATTR6 __attribute__((__cdecl__))
#define ATTR7
#define ATTR8 __attribute__((__thiscall__))

extern "C" void abort (void);

struct long_struct
{
  int a[3];
};

struct long_struct ret;

class c3 *this3;

class c1
{
  int val1;
public:
    virtual void foo () { }
};

class c2
{
public:
  virtual ATTR0 struct long_struct method0 ()
  {
    return ret;
  }

  virtual ATTR1 struct long_struct method1 ()
  {
    return ret;
  }

  virtual ATTR2 struct long_struct method2 ()
  {
    return ret;
  }

  virtual ATTR3 struct long_struct method3 ()
  {
    return ret;
  }

  virtual ATTR4 struct long_struct method4 ()
  {
    return ret;
  }

  virtual ATTR5 struct long_struct method5 ()
  {
    return ret;
  }

  virtual ATTR6 struct long_struct method6 ()
  {
    return ret;
  }

  virtual ATTR7 struct long_struct method7 ()
  {
    return ret;
  }

  virtual ATTR8 struct long_struct method8 ()
  {
    return ret;
  }
};

class c3:c1, public c2
{
public:
  c3 ()
  {
    this3 = this;
  }

  struct long_struct check_this (int a)
  {
    if (this3 != this)
      abort ();

    return ret;
  }

  virtual ATTR0 struct long_struct method0 ()
  {
    return check_this (0);
  }

  virtual ATTR1 struct long_struct method1 ()
  {
    return check_this (1);
  }

  virtual ATTR2 struct long_struct method2 ()
  {
    return check_this (2);
  }

  virtual ATTR3 struct long_struct method3 ()
  {
    return check_this (3);
  }

  virtual ATTR4 struct long_struct method4 ()
  {
    return check_this (4);
  }

  virtual ATTR5 struct long_struct method5 ()
  {
    return check_this (5);
  }

  virtual ATTR6 struct long_struct method6 ()
  {
    return check_this (6);
  }

  virtual ATTR7 struct long_struct method7 ()
  {
    return check_this (7);
  }

  virtual ATTR8 struct long_struct method8 ()
  {
    return check_this (7);
  }
};

class c3 c3_instance;
class c2 *c2_ptr = &c3_instance;

int
main ()
{
  c2_ptr->method0 ();
  c2_ptr->method1 ();
  c2_ptr->method2 ();
  c2_ptr->method3 ();
  c2_ptr->method4 ();
  c2_ptr->method5 ();
  c2_ptr->method6 ();
  c2_ptr->method7 ();
  c2_ptr->method8 ();

  return 0;
}
