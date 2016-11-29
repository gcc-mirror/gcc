// PR target/70098
// { dg-do compile }
// { dg-options -O2 }
// { dg-require-effective-target c++11 }

template < typename > struct traits;
template < typename, int _Rows, int _Cols, int = 0, int = _Rows,
	int = _Cols > class Matrix;
template < typename > class G;
template < typename Derived > struct A {
	typedef G < Derived > type;
};

template < typename Derived > class C {
public:
	enum { RowsAtCompileTime =
		    traits < Derived >::RowsAtCompileTime } static Zero;
};

template < typename Derived > class G:public C < Derived > {
};

template < int _Rows > class D {
public:
	long rows() {
		return _Rows;
	}
};

template < typename Derived > class PlainObjectBase:public A < Derived >::type {
	typedef typename A < Derived >::type Base;
	D < Base::RowsAtCompileTime > m_storage;

public:
	long rows() {
		return m_storage.rows();
	}
};

int fn1();

struct B {
	static long run(long x, long) {
		int offset(fn1());
		 return x + offset;
}};

long fn2(int x)
{
	return B::run(x, 0);
}

template < typename _Scalar, int _Rows, int _Cols, int _Options, int _MaxRows,
    int _MaxCols >
    struct traits <Matrix < _Scalar, _Rows, _Cols, _Options, _MaxRows, _MaxCols >> {
	enum { RowsAtCompileTime = _Rows };
};

template < typename, int, int, int, int _MaxRows, int _MaxCols >
	class Matrix:public PlainObjectBase < Matrix < double, _MaxRows,
	_MaxCols >> {
public:
	template < typename OtherDerived > Matrix(OtherDerived);
};

struct F {
	static Matrix < double, 2, 2 > run(long size) {
		Matrix < double, 2, 2 > diag = Matrix < double, 2, 2 >::Zero;
		long i = 0;
		while (i < size) {
			long randomInt = fn2(-1);
			if (randomInt == 0)
				++i;
			else {
				double alpha(randomInt);
				 diag = alpha;
				 i = 2;
			}
		}

		return diag;
	}
};

void fn3(Matrix < double, 2, 2 > m)
{
	long size = m.rows();
	F::run(size);
}
