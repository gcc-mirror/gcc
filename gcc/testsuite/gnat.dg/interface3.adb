--  { dg-do run }

procedure interface3 is
-- 
   package Pkg is
      type Foo is interface;
      subtype Element_Type is Foo'Class;
--    
      type Element_Access  is access Element_Type;
      type Elements_Type   is array (1 .. 1) of Element_Access;
      type Elements_Access is access Elements_Type;
--    
      type Vector is tagged record
         Elements : Elements_Access;
      end record;
--    
      procedure Test (Obj : Vector);
   end;
-- 
   package body Pkg is
      procedure Test (Obj : Vector) is
         Elements : Elements_Access := new Elements_Type;
--    
      begin
         Elements (1) := new Element_Type'(Obj.Elements (1).all);
      end;
   end;
--
begin
   null;
end;
