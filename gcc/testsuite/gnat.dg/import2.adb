--  { dg-do run }

procedure Import2 is
   type Index_Typ is mod 2**64;
   type Mod_Array is array (Index_Typ) of Integer;

   Obj : Mod_Array;
   pragma Import (Ada, Obj);
begin
   null;
end Import2;
