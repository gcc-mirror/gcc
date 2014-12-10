// { dg-do compile }
// { dg-additional-options "-std=gnu++11" }

enum class nsresult;
class A;
class B
{
public:
    B (int);
    A *operator->();
};
class C
{
};
class A
{
public:
    virtual nsresult AddObserver (const char *, C *, bool) = 0;
};
class D : A
{
  nsresult
      AddObserver (const char *p1, C *p2, bool p3)
	{
	  AddObserver (p1, p2, p3);
	}
};
char *prefList[]{};
class F : C
{
  nsresult Install ();
};
nsresult
F::Install ()
{
  B branch = 0;
  for (int i;;)
    branch->AddObserver (prefList[i], this, false);
}
