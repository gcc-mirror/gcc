-- { dg-do compile }
package body Old_Errors is

   A : Integer;

   function F
     (X : Integer := A'Old) -- { dg-error "can only appear within subprogram" }
     return Integer is
   begin
      return X;
   end F;

   procedure P (I : in Integer; O : out Integer; IO : in out Integer) is
      Y : Integer := 0;
      function G
        (X : Integer := Y'Old) -- { dg-error "cannot refer to local variable" }
        return Integer is
      begin
         return X;
      end G;

      function H (X : Integer := A'Old) return Integer is  -- OK
      begin
         return X;
      end H;

   begin
      Y := Y'Old; -- { dg-error "cannot refer to local variable" }
      declare
         Z : Integer := 0;
         procedure Inner is
            IL : Integer := 0;
         begin
            IL := IL'Old; -- { dg-error "cannot refer to local variable" }
            Z  := Z'Old;  -- OK
         end Inner;
      begin
         Y := Z'Old; -- { dg-error "cannot refer to local variable" }
      end;
      Y := I'Old;   -- { dg-warning "Old applied to constant has no effect" }
      Y := O'Old;   -- OK
      Y := IO'Old;  -- OK
      Y := G;       -- OK, error has been signalled at G declaration
      pragma Assert (G (3)'Old = Y); -- { dg-error "cannot refer to local variable" }
   end P;

end Old_Errors;
