--  { dg-do run }

with Ada.Text_IO, Ada.Tags;
procedure Test_Iface_Aggr is
   package Pkg is
     type Iface is interface;
     function Constructor (S: Iface) return Iface'Class is abstract;
     procedure Do_Test (It : Iface'class);
     type Root is abstract tagged record
        Comp_1 : Natural := 0; 
     end record;
     type DT_1 is new Root and Iface with record
         Comp_2, Comp_3 : Natural := 0;
     end record;
     function Constructor (S: DT_1) return Iface'Class;
     type DT_2 is new DT_1 with null record;  --  Test
     function Constructor (S: DT_2) return Iface'Class;
   end; 
   package body Pkg is
      procedure Do_Test (It: in Iface'Class) is
         Obj : Iface'Class := Constructor (It);
         S   : String := Ada.Tags.External_Tag (Obj'Tag);
      begin
         null;
      end;
     function Constructor (S: DT_1) return Iface'Class is
     begin
       return Iface'Class(DT_1'(others => <>));
     end;
     function Constructor (S: DT_2) return Iface'Class is
       Result : DT_2;
     begin
       return Iface'Class(DT_2'(others => <>));    --  Test
     end;
   end;
   use Pkg;
   Obj: DT_2;
begin
   Do_Test (Obj);
end;
