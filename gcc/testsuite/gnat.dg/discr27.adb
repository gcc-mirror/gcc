-- { dg-do compile }

package body Discr27 is

   subtype Index is Positive range 1..4096;

   function F return String is
      S : String(1..1) := (others =>'w');
   begin
      return S;
   end;

   type Enum is (One, Two);

   type Rec (D : Enum  := One; Len : Index := 1) is record
      case D is
          when One => I : Integer;
          when Two => A : String(1..Len);
      end case;
   end record;

   procedure Nothing is
      M : constant String := F;
      C : constant Rec := (Two, M'Length, M);
   begin
      null;
   end;

   procedure Proc is begin
      null;
   end;

end Discr27;
