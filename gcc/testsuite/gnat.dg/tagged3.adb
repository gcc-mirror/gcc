--  { dg-do run }

with Tagged3_Pkg; use Tagged3_Pkg;
procedure Tagged3 is
   package SP is
      type Ref is tagged private;

      procedure Set (Self : in out Ref'Class; Data : Parent'Class);

      type Reference_Type (Element : access Parent'Class)
         is limited null record with Implicit_Dereference => Element;

      function Get (Self : Ref'Class) return Reference_Type;

   private
      type Element_Access is access all Parent'Class;
      type Ref is tagged record
         Data : Element_Access;
      end record;
   end;

   package body SP is
      procedure Set (Self : in out Ref'Class; Data : Parent'Class) is
      begin
         Self.Data := new Parent'Class'(Data);
      end;

      function Get (Self : Ref'Class) return Reference_Type is
      begin
         return Reference_Type'(Element => Self.Data);
      end;
   end;

   DC : Child;
   RC : SP.Ref;
begin
   RC.Set (DC);
   Prim1 (RC.Get.Element); -- Test
   if not Tagged3_Pkg.Child_Prim1_Called then
     raise Program_Error;
   end if;
end;
