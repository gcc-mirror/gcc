-- { dg-do compile }

package body Renaming10 is

   function F (Input : Rec) return Natural is
      Position : Natural renames Input.Position;
      Index : Natural renames Natural'Succ(Position);
   begin
      return Index;
   end;

end Renaming10;
