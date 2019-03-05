package Ghost1 is
   type Ghost_Typ is record
      Data : Integer;
   end record
     with Ghost;

   procedure Spec_And_Body (Obj : Ghost_Typ)
     with Ghost;
end Ghost1;
