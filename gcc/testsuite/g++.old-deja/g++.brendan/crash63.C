// Build don't link: 
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

UnitList unit_list (String("keV"));
