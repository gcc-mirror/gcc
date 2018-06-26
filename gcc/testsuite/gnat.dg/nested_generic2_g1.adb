package body Nested_Generic2_G1 is

   procedure Debug (Msg : String; Prefix : String) is
   begin
      null;
   end;

   package body Nested is
      procedure Debug (Msg : String) is
      begin
         Debug (Msg, Prefix);
      end;
   end Nested;

end Nested_Generic2_G1;
