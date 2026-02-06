--  { dg-do run }

procedure Component_Value2 is

   type Bool_Packed_Array is array (Positive range 1 .. 20) of Boolean
     with Default_Component_Value => False, Pack;

   type Bool_Nonpacked_Array is array (Positive range 1 .. 20) of Boolean
     with Default_Component_Value => False;

   P  : Bool_Packed_Array;
   NP : Bool_Nonpacked_Array;

begin
   if not (for all I in P'Range => P(I) = False) then
      raise Program_Error;
   end if;

   if not (for all I in NP'Range => P(I) = False) then
      raise Program_Error;
   end if;
end;
