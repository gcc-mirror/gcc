package Tagged3_Pkg is
   type Parent is tagged null record;
   procedure Prim1 (Self : access Parent);

   type Child is new Parent with null record;
   procedure Prim1 (Self : access Child);

   Child_Prim1_Called : Boolean := False;
end;
