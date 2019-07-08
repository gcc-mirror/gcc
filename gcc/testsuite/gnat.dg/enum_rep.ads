with Interfaces;

package Enum_Rep is

   type My_Type is range 00 .. 100;

   subtype My_Subtype2 is Interfaces.Unsigned_32
        range My_Type'First'Enum_Rep .. My_Type'Last'Enum_Rep;

   My_Type_First : constant My_Type :=  My_Type'First;
   My_Type_Last : constant My_Type :=  My_Type'Last;

   subtype My_Subtype is Interfaces.Unsigned_32
         range My_Type_First'Enum_Rep .. My_Type_Last'Enum_Rep;

   subtype My_Subtype1 is Interfaces.Unsigned_32
        range My_Type'Enum_Rep (My_Type'First) ..
               My_Type'Enum_Rep (MY_Type'Last);

   procedure Foo;

end;
