// Origin: Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at>
// Special g++ Options: -O1

#include <map>
#include <cstdlib>

class NAMES_ITEM
    {
public:
    char *name;

      NAMES_ITEM(const NAMES_ITEM& item2);

      NAMES_ITEM(const char* name2);

      ~NAMES_ITEM();

      bool operator==(const NAMES_ITEM& n) const;
    };


NAMES_ITEM::NAMES_ITEM (const NAMES_ITEM& item2)
        {
        size_t length=strlen(item2.name);

        name=new char[length+1];
        memcpy(name,item2.name,length+1);
        }

NAMES_ITEM::NAMES_ITEM (const char* name2)      
        {
        size_t length=strlen(name2);

        name=new char[length+1];
        memcpy(name,name2,length+1);
        }

NAMES_ITEM::~NAMES_ITEM ()
{
  if (strcmp (name, "one") != 0)
    abort ();
  
  name=0;
}

bool NAMES_ITEM::operator==(const NAMES_ITEM& n) const
{
  return (strcmp(name,n.name) == 0);
}

bool operator<(const NAMES_ITEM& n1, const NAMES_ITEM& n2)
    {
    return (strcmp(n1.name,n2.name) < 0);
    }

    typedef map<NAMES_ITEM,size_t,less<NAMES_ITEM> > lookup_t;

    lookup_t lookup;

	NAMES_ITEM item ("one");
main()
  {
        lookup.insert(pair<NAMES_ITEM,size_t>(item,0));
  }

