// Build don't link:
// prms-id: 10046
template <class T>
class Array
{
public:
    T const & operator[] (int i) const;
};

class Referenceable {};

template < class Referencee >
class Referencer
{
public:
    operator Referencee *() const { return i_referencee; }

protected:
    Referencee* i_referencee;
};

class ScenarioGroup {};

class ScenarioSpace;

class ScenarioSet : public Referenceable,
		    public Array< Referencer<ScenarioGroup> >
{
public:
    ScenarioSet& operator=(ScenarioSet const & s);
};

class ScenarioSpace : public Referenceable,
		      public Array< Referencer<ScenarioSet> >
{
};

class ScenarioSetNameSelector
{
public:
    bool operator () (ScenarioSpace &space)
    {
	int idx;
	i_set = space[idx];
	return false;
    }
private:
    ScenarioSet *i_set;
};
