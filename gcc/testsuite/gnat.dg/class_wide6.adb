package body Class_Wide6 is

   function Parse (Parser: Script_Info_Parser) return Script_Info'Class is
   begin
      pragma Warnings(Off);
      return Parse (Parser);
   end;

end Class_Wide6;
