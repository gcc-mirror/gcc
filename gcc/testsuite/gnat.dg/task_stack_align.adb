-- { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements; use System.Storage_Elements;

procedure Task_Stack_Align is

   type Align_Me is record
      Value : Integer;
   end record;
   for Align_Me'Alignment use Standard'Maximum_Alignment;

   procedure Check_Local_Alignment_From (Context : String) is
      Object : Align_Me;
   begin
      if To_Integer (Object'Address) mod Object'Alignment /= 0 then
         Put_Line ("alignment check failed in " & Context);
      end if;
   end;

   task type T;

   task body T is
   begin
      Check_Local_Alignment_From ("task T");
   end;

   Tasks : array (1 .. 50) of T;
begin
   Check_Local_Alignment_From ("environment");
end;
