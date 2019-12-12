
package Inline17_Pkg3 is

   type SQL_Field is tagged null record;

   function "+" (Field : SQL_Field'Class) return Integer;

   type Ref is record
      Data : Integer;
   end record;

   function Unchecked_Get (Self : Ref) return Integer with Inline_Always;

   function Get (Self : Ref) return Integer is (Unchecked_Get (Self));

end Inline17_Pkg3;
