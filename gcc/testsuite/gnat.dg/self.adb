package body Self is 
   function G (X : Integer) return Lim is
   begin   
      return R : Lim := (Comp => X, others => <>); 
   end G;  

   procedure Change (X : in out Lim; Incr : Integer) is
   begin   
      X.Comp := X.Comp + Incr; 
      X.Self_Default.Comp := X.Comp + Incr; 
      X.Self_Anon_Default.Comp := X.Comp + Incr; 
   end Change; 

   function Get (X : Lim) return Integer is
   begin   
      return X.Comp; 
   end;    
end Self;
