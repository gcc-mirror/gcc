with GNAT.Dynamic_Tables;

package Pack18_Pkg is

   type String_Access is access String;

   type Rec is record
      S : String_Access;
      B : Boolean;
      N : Natural;
   end record;
   pragma Pack (Rec);

   package Attributes_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Rec,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 200);

end Pack18_Pkg;
