// { dg-do assemble  }

#include <typeinfo>

struct GcspFlags
{
  enum Enum
  { 
    OffYes, 
    OffNo, 
    Root
  };

  static char const* name(Enum flag);

  template<Enum GCSP_FLAG>
  struct btmFlag
  { 
    static Enum const f=OffNo;
  };

};

template<>
struct 
GcspFlags::btmFlag<GcspFlags::OffYes>
{ 
  static GcspFlags::Enum const f=GcspFlags::OffYes;
};

int
main ()
{
  GcspFlags::btmFlag<GcspFlags::OffYes> f;
  const std::type_info& ti = typeid (f);
  return 0;
}

