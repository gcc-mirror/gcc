// { dg-do assemble  }
// GROUPS passed enums
class foo1
{
 public:
   enum foo1_enum
   {
      ENUM1,
      ENUM2, // { dg-error "comma at end" "" { target { ! c++11 } } }
   };
};

   
class foo2 
{
 private:
   enum foo1::foo1_enum	Enum;
};


class foo3
{
 private:
   foo1::foo1_enum Enum;
};
