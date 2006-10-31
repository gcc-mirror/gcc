-- { dg-do run }

with Text_IO; use Text_IO;
with Ada.Finalization; use Ada.Finalization;

procedure Nested_Controlled_Alloc is
   
   package Controlled_Alloc is

      type Fin is new Limited_Controlled with null record;
      procedure Finalize (X : in out Fin);

      F : Fin;
      
      type T is limited private;
      type Ref is access all T;
   
   private
      
      type T is new Limited_Controlled with null record;
      procedure Finalize (X : in out T);
   
   end Controlled_Alloc;
   
   package body Controlled_Alloc is

       procedure Finalize (X : in out T) is
       begin
          Put_Line ("Finalize (T)");
       end Finalize;

       procedure Finalize (X : in out Fin) is
          R : Ref;
       begin
          begin
             R := new T;
             raise Constraint_Error;
          
          exception
             when Program_Error =>
                null;  -- OK
          end;
       end Finalize;
   
   end Controlled_Alloc;

begin
   null;
end Nested_Controlled_Alloc;
