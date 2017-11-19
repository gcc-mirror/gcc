// { dg-do compile }
// { dg-additional-options "-fstack-clash-protection" }
// { dg-require-effective-target supports_stack_clash_protection }


class a
{
public:
  ~a ();
  int b;
};
class c
{
public:
  a m_fn1 ();
};
class d
{
  int e ();
  c f;
};
int
d::e ()
{
  return f.m_fn1 ().b;
}
