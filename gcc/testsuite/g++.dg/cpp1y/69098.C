// PR c++/69098
// { dg-do compile { target c++14 } }

template<typename> struct SpecPerType;

class Specializer
{
public:
    template<bool> void MbrFnTempl() //Must be a template
	{
	}
	template<unsigned> struct InnerClassTempl
	{  //Had to be a template whenever I tested for it
		static void InnerMemberFn();
	};

	void Trigger()
	{
		InnerClassTempl<0u>::InnerMemberFn();
	}
};

template<> struct SpecPerType<Specializer>
{
	using FnType = void (Specializer::*)();
    template<bool P> static constexpr FnType SpecMbrFnPtr =
        &Specializer::template MbrFnTempl<P>;
};

template<bool> constexpr SpecPerType<Specializer>::FnType
    SpecPerType<Specializer>::SpecMbrFnPtr; //Just a formalism

template<unsigned X> void Specializer::InnerClassTempl<X>::InnerMemberFn()
{
	using Spec = SpecPerType<Specializer>;
	typename Spec::FnType ErrorSite = Spec::template SpecMbrFnPtr<true>;
    //ErrorSite would get called next in the original code
    //(this should result in a call to MbrFnTempl)
}

int main()
{
}
