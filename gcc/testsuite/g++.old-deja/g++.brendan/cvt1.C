// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed conversions
#include <iostream>

class Thing
{
public:
      typedef enum { GOOD_THING, BAD_THING, } ThingType ; // { dg-error "" "comma" { target { ! c++11 } } }
	Thing (ThingType type) : thingType (type) { }
	~Thing () { }
private:
	ThingType thingType ;
} ;

class Group
{
public:
      typedef enum { THIS_GROUP, THAT_GROUP, } GroupType ; // { dg-error "" "comma" { target { ! c++11 } } }
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
	std::cout << "Group type is " << (Group::GroupType) g << std::endl ;
	return 0 ;
}
