// { dg-do assemble  }
// GROUPS passed old-abort
class String
   {
 public:
   String (const char *str);
   String (const String&);
   };

class UnitList 
   {
 public:
   UnitList (...);
   };

UnitList unit_list (String("keV")); // { dg-error "" } cannot pass non-pod
