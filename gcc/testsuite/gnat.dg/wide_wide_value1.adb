--  { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;

procedure Wide_Wide_Value1 is
begin
   begin
      declare
         Str : constant Wide_Wide_String :=
                 Wide_Wide_Character'Val (16#00000411#) &
                 Wide_Wide_Character'Val (16#0000043e#) &
                 Wide_Wide_Character'Val (16#00000434#) &
                 Wide_Wide_Character'Val (16#00000430#) &
                 Wide_Wide_Character'Val (16#00000443#) &
                 Wide_Wide_Character'Val (16#00000431#) &
                 Wide_Wide_Character'Val (16#00000430#) &
                 Wide_Wide_Character'Val (16#00000435#) &
                 Wide_Wide_Character'Val (16#00000432#) &
                 Wide_Wide_Character'Val (16#00000416#) &
                 Wide_Wide_Character'Val (16#00000443#) &
                 Wide_Wide_Character'Val (16#0000043c#) &
                 Wide_Wide_Character'Val (16#00000430#) &
                 Wide_Wide_Character'Val (16#00000442#) &
                 Wide_Wide_Character'Val (16#0000041c#) &
                 Wide_Wide_Character'Val (16#00000430#) &
                 Wide_Wide_Character'Val (16#00000440#) &
                 Wide_Wide_Character'Val (16#00000430#) &
                 Wide_Wide_Character'Val (16#00000442#) &
                 Wide_Wide_Character'Val (16#0000043e#) &
                 Wide_Wide_Character'Val (16#00000432#) &
                 Wide_Wide_Character'Val (16#00000438#) &
                 Wide_Wide_Character'Val (16#00000447#);

         Val : constant Integer := Integer'Wide_Wide_Value (Str);
      begin
         Put_Line ("ERROR: 1: Constraint_Error not raised");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Put_Line ("ERROR: 1: unexpected exception");
   end;

   begin
      declare
         Str : Wide_Wide_String (1 .. 128) :=
                 (others => Wide_Wide_Character'Val (16#0FFFFFFF#));

         Val : constant Integer := Integer'Wide_Wide_Value (Str);
      begin
         Put_Line ("ERROR: 1: Constraint_Error not raised");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Put_Line ("ERROR: 1: unexpected exception");
   end;
end Wide_Wide_Value1;
