--  { dg-do compile }
--  { dg-options "-gnateV" }

procedure Unchecked_Union4 is

   type R (Bytes_Mode : Boolean := False) is record
      case Bytes_Mode is
         when True =>
            A : Boolean;
         when False =>
            B : Boolean;
      end case;
   end record with Unchecked_Union;

   function F (Message : R) return Integer is (0);

begin
   null;
end;
