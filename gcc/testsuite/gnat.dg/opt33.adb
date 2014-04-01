-- { dg-do compile }
-- { dg-options "-O" }

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;

procedure Opt33 is

   type Rec is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function "<" (Left : Rec; Right : Rec) return Boolean;

   package My_Ordered_Sets is new Ada.Containers.Ordered_Sets (Rec);

   protected type Data is
      procedure Do_It;
   private
      Set : My_Ordered_Sets.Set;
   end Data;

   function "<" (Left : Rec; Right : Rec) return Boolean is
   begin
      return False;
   end "<";

   protected body Data is
      procedure Do_It is
         procedure Dummy (Position : My_Ordered_Sets.Cursor) is
         begin
            null;
         end;
      begin
         Set.Iterate (Dummy'Access);
      end;
   end Data;

begin
   null;
end;
