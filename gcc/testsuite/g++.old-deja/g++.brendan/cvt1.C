// Build don't link: 
// GROUPS passed conversions
#include <iostream.h>

class Thing
{
public:
      typedef enum { GOOD_THING, BAD_THING, } ThingType ; // ERROR - comma
	Thing (ThingType type) : thingType (type) { }
	~Thing () { }
private:
	ThingType thingType ;
} ;

class Group
{
public:
      typedef enum { THIS_GROUP, THAT_GROUP, } GroupType ; // ERROR - comma
	Group (GroupType type) : groupType (type), groupCount (0) { }
	~Group () { }
	void append (Thing* const &entry) { groupCount ++ ; }
	operator GroupType () const { return groupType ; }
	operator int () const { return groupCount ; } // remove this and problem gone

private:
	int groupCount ;
	GroupType groupType ;
} ;

inline Group& operator += (Group& g, Thing* const t)
{
	g.append (t) ;
	return g ; // complaint is here
}

int
main (int argc, char** argv)
{
	Group g (Group::THIS_GROUP) ;

	g += new Thing (Thing::GOOD_THING) ;
	cout << "Group type is " << (Group::GroupType) g << endl ;
	return 0 ;
}
