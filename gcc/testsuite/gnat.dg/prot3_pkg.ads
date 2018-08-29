package Prot3_Pkg is
   
   type Rec is record
      V1 : Short_Integer;
      V2 : Short_Integer;
   end record with Volatile_Full_Access;
   
   protected type Prot is
      procedure Foo (J : Short_Integer);
   private
      Val : Rec;
   end Prot;
   
   P : Prot;
   
end Prot3_Pkg;
