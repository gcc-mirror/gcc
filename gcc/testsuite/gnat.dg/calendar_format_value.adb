-- { dg-do run }
-- { dg-options "-O2" }

with Ada.Calendar.Formatting;

procedure Calendar_Format_Value is
   Limit : constant Duration
     := 99 * 3600.0 + 59 * 60.0 + 59.0;
begin
   declare
      Image : constant String := Ada.Calendar.Formatting .Image (Limit);
      Image_Error : exception;
   begin
      if Image /= "99:59:59" then
         raise Image_Error with "image: " & Image;
      end if;
      declare
         Value : constant Duration := Ada.Calendar.Formatting.Value (Image);
         Value_Error : exception;
      begin
         if Value /= Limit then
            raise Value_Error with "duration: " & Value'Image;
         end if;
      end;
   end;
end Calendar_Format_Value;
