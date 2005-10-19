/* { dg-do compile } */
/* { dg-options "-O2" } */
/* FRE testcase for PR 24231, problem with PRE coalescing abnormal phis.  */
struct f
{
  int i;
};
struct h{h();};
int g(void);
int g1(void) throw();
int h2222(f*);
void ghh(int);

int main(void)
{
  int i;
  f t;
  try
  {
    i = g1();
    try
    {
      i = g();
    }catch(...)
    {}
    int j = i;
    try
    { t.i = i;
      i = g();
    }catch(...)
   {}
    i = 2;
    int h = t.i;
    ghh (h);

    g();
  }catch(...)
  {}
  return i;
}


