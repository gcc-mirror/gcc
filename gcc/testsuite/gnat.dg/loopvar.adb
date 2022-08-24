--  { dg-do compile }

procedure Loopvar (S : String) is
   J : Integer := S'First;
begin
   while J < S'Last loop
      pragma Loop_Variant (J);               -- { dg-error "expect name \"Increases\"" }
      pragma Loop_Variant (Increasing => J); -- { dg-error "expect name \"Increases\"" }
      pragma Loop_Variant (J + 1);           -- { dg-error "expect name \"Increases\"" }
      pragma Loop_Variant (incr => -J + 1);  -- { dg-error "expect name \"Increases\"" }
      pragma Loop_Variant (decr => -J + 1);  -- { dg-error "expect name \"Decreases\"" }
      pragma Loop_Variant (foof => -J + 1);  -- { dg-error "expect name \"Increases\", \"Decreases\", or \"Structural\"" }
      J := J + 2;
   end loop;
end Loopvar;
