// { dg-do assemble  }
// GROUPS passed old-abort
class String
   {
 public:
   String (const char *str);
   };

class UnitList 
   {
 public:
   UnitList (...);
   };

UnitList unit_list (String("keV")); // { dg-warning "" } cannot pass non-pod
