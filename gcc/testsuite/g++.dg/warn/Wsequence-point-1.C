// { dg-do compile }
// { dg-options "-Wsequence-point" }
struct C
{
  ~C() throw();
};

void t_test1 (C* mapping)
{
  delete [] mapping;
}
