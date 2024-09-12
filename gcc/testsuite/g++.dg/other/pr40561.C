// { dg-do compile }
// { dg-skip-if "requires hosted libstdc++ for set" { ! hostedlib } }

#include <set>

class SyAccess;
class VamsBase 
{
	public:
	virtual ~VamsBase(void);
};

class VamsFnct : public VamsBase 
{	
	public:
	~VamsFnct(void);
	std::set<SyAccess*> getNullDependencies(void) const
	{	return std::set<SyAccess*>();
	}
};

class VamsFnctSystem:public VamsFnct
{	public:
	VamsFnctSystem(
		const bool _bPassDependencies);
};

template< std::set<SyAccess*> (VamsFnct::*__GET_DEP__)(void) const >
class VamsSystemFunction:public VamsFnctSystem
{	public:
	VamsSystemFunction()
	    :VamsFnctSystem(
		__GET_DEP__ != &VamsFnct::getNullDependencies
		)
	{
	}
};

VamsSystemFunction<&VamsFnct::getNullDependencies> s;
