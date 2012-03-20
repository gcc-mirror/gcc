with Pack8_Pkg;

package Pack8 is

   subtype Index_Type is Integer range 1 .. Pack8_Pkg.N;

   subtype Str is String( Index_Type);

   subtype Str2 is String (1 .. 11);

   type Rec is record
      S1 : Str;
      S2 : Str;
      B  : Boolean;
      S3 : Str2;
   end record;
   pragma Pack (Rec);

end Pack8;
