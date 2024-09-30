-- { dg-do link }
-- { dg-options "-O2 -gnatp -flto" { target lto } }

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

procedure Lto27 is
   subtype Node_Name is String (1 .. 4);

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Node_Name,
      Element_Type    => Integer,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

begin
   null;
end;
