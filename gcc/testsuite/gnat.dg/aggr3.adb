--  { dg-do run }

with Ada.Tags;    use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
procedure aggr3 is
   package Pkg is
      type Element is interface;
      type Event is tagged record
         V1 : Natural;
         V2 : Natural;
      end record;
      function Create return Event;
      type D_Event is new Event and Element with null record;
      function Create return D_Event;
   end;
   package body Pkg is
      function Create return Event is
         Obj : Event;
      begin
         Obj.V1 := 0;
         return Obj;
      end;
      function Create return D_Event is
      begin
         return (Event'(Create) with null record);
      end;
   end;
   use Pkg;
   procedure CW_Test (Obj : Element'Class) is
      S : Constant String := Expanded_Name (Obj'Tag);
   begin
      null;
   end;
begin
   CW_Test (Create);
end;
