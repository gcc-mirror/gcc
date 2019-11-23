// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized-vops -fno-inline-functions --param max-inline-insns-single=200" }

struct VBase;

//Very minimal numeric vector class where Base provides the policy
template<typename Base=VBase>
struct Vector : public Base{
	
	inline Vector(const Base& b)
	:Base(b)
	{
	}

	//Assignment from any other sort of Vector
	template<typename Base2>
	void operator= (const Vector<Base2>& from)
	{
		for(int i=0; i<100; i++){
			(*this)[i]=from[i];
		}
	}
};


//Base class to represent pointer as a Vector
struct VBase{
	double * const my_data;

	double& operator[](int i) {
		return my_data[i];
	}

	const double& operator[](int i) const {
		return my_data[i];
	}
};

//Base class providing very minimalistic expression template
template<class B2> struct ScalarMulExpr
{
	const int& mul;
	const Vector<B2>& vec;

	int size() const
	{
		return vec.size();
	}

	double operator[](int i) const
	{
		return vec[i]*mul;
	}

	ScalarMulExpr(const Vector<B2>& vec_, const int& m)
	:mul(m),vec(vec_)
	{
	}
};

//Allow vector to be multiplied by a scalar
template<class B2>
Vector<ScalarMulExpr<B2> > operator*(const Vector<B2>& lhs, const int& rhs)
{
	return ScalarMulExpr<B2>(lhs, rhs);
}

//Test function producing suboptimal asm code
void test(const Vector<>& in, Vector<>& out, int i)
{
	out=in*1*1*1*1*1*1*1*1*1*1*1;
}

// There should be a single store remaining, inside the loops.  All
// dead stores to unused temporaries should have been removed.

// { dg-final { scan-tree-dump-times "VDEF" 1 "optimized" } }
