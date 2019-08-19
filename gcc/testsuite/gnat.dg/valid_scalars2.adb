--  { dg-do run }
--  { dg-options "-O0 -gnata -gnateV" }

with Ada.Exceptions; use Ada.Exceptions;

procedure Valid_Scalars2 is

   Traced : Boolean := False;

   procedure Trace (E : in Exception_Occurrence) is
      pragma Assert (E'Valid_scalars);
   begin
      Traced := True;
   end Trace;

begin
   raise Program_Error;
exception
   when E : others =>
      pragma Assert (E'Valid_scalars);
      Trace (E);
      if not Traced then
         raise Program_Error;
      end if;
end Valid_Scalars2;
