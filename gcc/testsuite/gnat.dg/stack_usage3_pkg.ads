package Stack_Usage3_Pkg is

   subtype Small_String is String (1..80);

   function Diag (S : String) return Small_String;

end Stack_Usage3_Pkg;
