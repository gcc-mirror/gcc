with Text_IO; use Text_IO;

package body Concat2 is

   function Get_Param return String is
   begin
      return "";
   end;

   procedure Browse is
      Mode         : constant String := Get_Param;
      Mode_Param   : constant String := "MODE=" & Mode;
   begin
      Put_Line (Mode_Param);
   end;

end Concat2;

