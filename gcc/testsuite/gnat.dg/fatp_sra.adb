--  { dg-do compile }
--  { dg-options "-gnatp -O1" }

procedure Fatp_Sra is

   function X return String is
   begin
      return "X";
   end;

   function Letter return String is
   begin
      return X;
   end;
begin
   null;
end;
