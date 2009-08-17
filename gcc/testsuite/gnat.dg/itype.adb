package body itype is
   function G return not null access constant T is
      X : aliased T;
   
   begin
      return X'Unchecked_Access;
   end G;
end itype;
