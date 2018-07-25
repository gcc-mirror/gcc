/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-fPIC -flto -flto-partition=1to1 -r -nostdlib}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

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
  return 0;
}
