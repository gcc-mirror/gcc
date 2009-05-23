package Rep_Clause3 is

  type Record1 is
      record
         Page_Handle : Integer range 0 .. 255;
         Page_Owner :  Integer range 0 .. 15;
      end record;
   for Record1 use
      record
         Page_Handle at 0 range 0 .. 15;
         Page_Owner at 0 range 16 .. 19;
      end record;
   for Record1'Size use 20;

   type Range_A is range 1 .. 7;
   for Range_A'Size use 16;

   type Array_Type is array (Range_A) of Record1;
   pragma Pack (Array_Type);
   for Array_Type'Size use 7 * 20;
--   for array_Type'alignment use 1;

   type Record2 is
      record
         Page_Tree_Index : Range_A;
         Page_Tree : Array_Type;
      end record;

   for Record2 use
      record
         Page_Tree_Index at 0 range 0 .. 15;
         Page_Tree at 0 range 16 .. 15 + (7 * 20);
      end record;
   for Record2'Size use 16 + (7 * 20);

end Rep_Clause3;
