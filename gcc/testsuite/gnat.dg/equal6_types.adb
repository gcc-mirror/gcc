package body Equal6_Types is

   function "=" (L, R : in Mail_Box_Data_T) return Boolean is
      use type Bits_T;
   begin
      Equal1_Called := True;
      return L.Message (1) = R.Message (1);
   end "=";

   function "=" (L, R : in To_Evc_Optional_Packet_List_T) return Boolean is
   begin
      Equal2_Called := True;
      return L.List (1) = R.List (1);
   end "=";
end Equal6_Types;
