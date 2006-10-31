package body generic_dispatch_p is
   function Constructor (I : not null access Integer) return DT is
      R : DT; 
  begin
      return R;
   end Constructor;
end;
