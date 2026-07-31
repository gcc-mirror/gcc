--  { dg-do compile }
--  { dg-options "-gnatVa" }

procedure Validity_Check3 is

   type Selection is (First, Second);

   function Next_Value (Text     : String;
                        Position : in out Positive) return String is
   begin
      Position := Position + 1;
      return Text;
   end Next_Value;

   Position : Positive := 1;
   Value    : constant Selection :=
     Selection'Value (Next_Value ("First", Position));

begin
   if Value /= First then
      raise Program_Error;
   end if;
end;
