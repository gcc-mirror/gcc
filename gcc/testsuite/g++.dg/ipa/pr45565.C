// { dg-do compile }
// { dg-options "-O -fno-toplevel-reorder -fno-inline -fipa-cp -fipa-cp-clone -fkeep-inline-functions" }

template < typename Derived > struct AnyMatrixBase
{
};

struct Matrix Random ();

struct Matrix:AnyMatrixBase < Matrix >
{
  void bar ()
    {
      throw;
    }
  void foo (Matrix other)
    {
      bar ();
      Matrix (AnyMatrixBase < Matrix > (Random ()));
    }
  template
      < typename OtherDerived > Matrix (AnyMatrixBase < OtherDerived > other)
	{
	  foo (other);
	}
};

Matrix x (Random ());

