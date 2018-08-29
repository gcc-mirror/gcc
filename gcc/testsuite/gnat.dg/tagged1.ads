with Ada.Containers.Vectors;
with Ada.Containers;
with Ada.Finalization;

package Tagged1 is

   generic
      type Target_Type (<>) is limited private;
   package A is
      type Smart_Pointer_Type is private;
   private
      type Smart_Pointer_Type
        is new Ada.Finalization.Controlled with null record;
   end;

   generic
      type Target_Type (<>) is limited private;
   package SP is
      type Smart_Pointer_Type is private;
   private
      package S is new A (Integer);
      type Smart_Pointer_Type is new S.Smart_Pointer_Type;
   end;

   type Root_Type is tagged record
      Orders : Integer;
   end record;
   package Smarts is new SP
     (Target_Type => Root_Type'Class);

   type Fat_Reference_Type is new Smarts.Smart_Pointer_Type;
   type EST is record
      Orders : Fat_Reference_Type;
   end record;

   package V is new Ada.Containers.Vectors (Positive, EST);

   procedure Dummy;
end;
