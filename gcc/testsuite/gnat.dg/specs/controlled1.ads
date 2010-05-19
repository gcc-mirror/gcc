--  { dg-do compile }

with Ada.Finalization;
with Controlled1_Pkg; use Controlled1_Pkg;

package Controlled1 is

   type Collection is new Ada.Finalization.Controlled with null record;

   type Object_Kind_Type is (One, Two);

   type Byte_Array is array (Natural range <>) of Integer;

   type Bounded_Byte_Array_Type is record
     A : Byte_Array (1 .. Value);
   end record;

   type Object_Type is tagged record
     A : Bounded_Byte_Array_Type;
   end record;

   type R_Object_Type is new Object_Type with record
      L : Collection;
   end record;

   type Obj_Type (Kind : Object_Kind_Type := One) is record
      case Kind is
         when One => R : R_Object_Type;
         when others => null;
      end case;
   end record;

   type Obj_Array_Type is array (Positive range <>) of Obj_Type;

end Controlled1;
