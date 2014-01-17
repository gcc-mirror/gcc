
/* PR target/59695 */
/* { dg-do run } */
/* { dg-options "-O0" } */

#define  DEFINE_VIRTUALS_FNS(i)	virtual void  xxx##i ()	{} \
  virtual void  foo1_##i ()	{}\
  virtual void  foo2_##i ()	{}\
  virtual void  foo3_##i ()	{}\
  virtual void  foo4_##i ()	{}\
  virtual void  foo5_##i ()	{}\
  virtual void  foo6_##i ()	{}\
  virtual void  foo7_##i ()	{}\
  virtual void  foo8_##i ()	{}\
  virtual void  foo9_##i ()	{}\
  virtual void  foo10_##i ()	{}\
  virtual void  foo11_##i ()	{}\
  virtual void  foo12_##i ()	{}\
  virtual void  foo13_##i ()	{}\
  virtual void  foo14_##i ()	{}\
  virtual void  foo15_##i ()	{}\
  virtual void  foo16_##i ()	{}\
  virtual void  foo17_##i ()	{}\
  virtual void  foo18_##i ()	{}\
  virtual void  foo19_##i ()	{}\
  virtual void  foo20_##i ()	{}\
  virtual void  foo21_##i ()	{}\
  virtual void  foo22_##i ()	{}\

class base_class_2
{

public:
  /* Define lots of virtual functions */
  DEFINE_VIRTUALS_FNS (1)
  DEFINE_VIRTUALS_FNS (2)
  DEFINE_VIRTUALS_FNS (3)
  DEFINE_VIRTUALS_FNS (4)
  DEFINE_VIRTUALS_FNS (5)
  DEFINE_VIRTUALS_FNS (6)
  DEFINE_VIRTUALS_FNS (7)
  DEFINE_VIRTUALS_FNS (8)
  DEFINE_VIRTUALS_FNS (9)
  DEFINE_VIRTUALS_FNS (10)
  DEFINE_VIRTUALS_FNS (11)
  DEFINE_VIRTUALS_FNS (12)
  DEFINE_VIRTUALS_FNS (13)
  DEFINE_VIRTUALS_FNS (14)
  DEFINE_VIRTUALS_FNS (15)
  DEFINE_VIRTUALS_FNS (16)
  DEFINE_VIRTUALS_FNS (17)
  DEFINE_VIRTUALS_FNS (18)
  DEFINE_VIRTUALS_FNS (19)
  DEFINE_VIRTUALS_FNS (20)

  base_class_2();
  virtual ~base_class_2 ();
};

base_class_2::base_class_2()
{
}

base_class_2::~base_class_2 ()
{
}

class base_class_1
{
public:
  virtual ~base_class_1();
  base_class_1();
};

base_class_1::base_class_1()
{
}

base_class_1::~base_class_1()
{
}

class base_Impl_class :
  virtual public base_class_2, public base_class_1
{
public:
  base_Impl_class ();
  virtual ~base_Impl_class ();
};

base_Impl_class::base_Impl_class ()
{
}

base_Impl_class::~base_Impl_class ()
{
}


class test_cls : public base_Impl_class
{
public:
  test_cls();
  virtual ~test_cls();
};

test_cls::test_cls()
{
}

test_cls::~test_cls()
{
}

int main()
{
  test_cls *test = new test_cls;
  base_class_2 *p1 = test;

  /* PR59695  destructor thunk offsets are not setup
   correctly resulting in crash.  */
  delete p1;
  return 0;
}

