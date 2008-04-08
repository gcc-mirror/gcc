--  { dg-do compile }

package body RT1 is
   procedure P (S : access Root_Stream_Type'Class) is
      Val : constant Ptr := Ptr'Input (S);
   begin
      null;
   end P;
end RT1;
