// { dg-do assemble  }
// prms-id: 10038
template < class Referencee >
class Referencer
{
public:
    Referencer (Referencee const * pReferencee);
};

template <class T>
class Array
{
public:
    int addElement (T const & e);
    int addElement ();
};

class ScenarioGroup;

class ScenarioSet : public Array< Referencer<ScenarioGroup> >
{
    typedef Array< Referencer<ScenarioGroup> > arrayBase;
    void addElement(ScenarioGroup *group)
	{
	    arrayBase::addElement(group);
	}
};
