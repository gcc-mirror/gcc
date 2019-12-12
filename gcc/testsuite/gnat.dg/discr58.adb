--  { dg-do compile }

with Ada.Text_IO; use Ada.Text_IO;

procedure Discr58 is

   type Field(Flag : Boolean := True) is record
      case Flag is
         when True  => Param1 : Boolean := False;
         when False => Param2 : Boolean := True;
      end case;
   end record;

   type Header(Flag : Boolean := True) is record
      Param3 : Integer     := 0;
      Params : Field(Flag) := (if Flag = True then
                                  (Flag => True, others => <>)
                               else
                                  (Flag => False, others => <>));
   end record;

   type Message(Flag : Boolean) is record

      -- This assignment crashes GNAT
      The_Header : Header(Flag) := Header'(Flag => True, others => <>);
   end record;

   It : Message (True);
begin
   Put_Line("Hello World");
   Put_Line (Boolean'Image (It.The_Header.Flag));
   Put_Line (Boolean'Image (It.The_Header.Params.Flag));
end Discr58;