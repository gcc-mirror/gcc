--  { dg-do compile }

with System; with Ada.Unchecked_Conversion;
procedure Test_Call is
   type F_ACC is access function (Str : String) return String;
        
   function Do_Something (V : F_Acc) return System.Address is
   begin
      return System.Null_Address;
   end Do_Something;

   function BUG_1 (This : access Integer) return F_Acc is
   begin
      return null;
   end BUG_1;

   function Unch is new Ada.Unchecked_Conversion (F_Acc, System.Address);
   Func : System.Address := Unch (BUG_1 (null));

   V : System.Address := Do_Something (BUG_1 (null));

begin
   null;
end Test_Call;
