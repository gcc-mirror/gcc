generic
   S : String;
package Nested_Generic2_G1 is

   procedure Debug (Msg : String; Prefix : String);

   generic
      Prefix : String;
   package Nested is
      procedure Debug (Msg : String);
   end Nested;

end Nested_Generic2_G1;
