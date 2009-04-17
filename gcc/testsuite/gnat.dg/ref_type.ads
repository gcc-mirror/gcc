package ref_type is
private
   type T is tagged null record;
   procedure Print (X : T);
end ref_type;
