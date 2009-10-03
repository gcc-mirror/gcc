/* { dg-lto-do link } */
/* { dg-lto-options {{-fPIC -fwhopr -shared}} } */

/* We used to ICE because of dangling pointers.  */

class object
{
public:
  virtual ~object() {}
};

class foo : public object
{
  virtual int method(void);
};

int
foo::method(void)
{
}
