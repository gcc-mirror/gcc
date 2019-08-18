package Default_Initial_Condition_Pack is
   type T;
   type T is private
     with Default_Initial_Condition => Is_OK (T);

   function Is_OK (Val : T) return Boolean;

   DIC_Called : Boolean := False;

private
   type T is null record;
end Default_Initial_Condition_Pack;
