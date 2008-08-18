-- { dg-do run }
with GNAT.Compiler_Version;
procedure Test_Version is
   package Vsn is new GNAT.Compiler_Version;
   use Vsn;
   X : constant String := Version;
begin
   if X'Length = 46 then
      -- 46 = Ver_Len_Max + Ver_Prefix
      -- actual version should be shorter than this
      raise Program_Error;
   end if;
end Test_Version;
