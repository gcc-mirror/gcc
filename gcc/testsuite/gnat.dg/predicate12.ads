package Predicate12 is

   subtype Index_Type is Positive range 1 .. 100;
   type Array_Type is array(Index_Type) of Integer;

   type Search_Engine is interface;

   procedure Search
     (S           : in  Search_Engine;
      Search_Item : in  Integer;
      Items       : in  Array_Type;
      Found       : out Boolean;
      Result      : out Index_Type) is abstract
     with
       Pre'Class =>
         (for all J in Items'Range =>
           (for all K in J + 1 .. Items'Last => Items(J) <= Items(K))),
       Post'Class =>
         (if Found then Search_Item = Items(Result)
                   else (for all J in Items'Range => Items(J) /= Search_Item));

   type Binary_Search_Engine is new Search_Engine with null record;

   procedure Search
     (S           : in  Binary_Search_Engine;
      Search_Item : in  Integer;
      Items       : in  Array_Type;
      Found       : out Boolean;
      Result      : out Index_Type) is null;

   type Forward_Search_Engine is new Search_Engine with null record;

   procedure Search
     (S           : in  Forward_Search_Engine;
      Search_Item : in  Integer;
      Items       : in  Array_Type;
      Found       : out Boolean;
      Result      : out Index_Type) is null;

   procedure Dummy;

end Predicate12;
