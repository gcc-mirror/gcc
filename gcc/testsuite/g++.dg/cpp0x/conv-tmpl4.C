// PR c++/93870 - wrong error when converting template non-type arg.
// { dg-do compile { target c++11 } }

template <typename ENUM> struct EnumWrapper
{
	ENUM value;

	constexpr operator ENUM() const
	{
		return value;
	}
};

enum E : int { V };

constexpr EnumWrapper<E> operator ~(E a)
{
    return {E(~int(a))};
}

template <E X> struct R
{
    static void Func();
};

template <E X> struct S : R<~X>
{
};

void Test()
{
    S<E::V>::Func();
}
