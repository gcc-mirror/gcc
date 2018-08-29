package body Prot3_Pkg is
   
   protected body Prot is
      function Fn (J : Short_Integer) return Rec
      is
      begin
	 return (V1 => J * J,
		 V2 => J);
      end;
      
      procedure Foo (J : Short_Integer) is
      begin
	 Val := Fn (J);
      end;
   end Prot;
   
end Prot3_Pkg;
