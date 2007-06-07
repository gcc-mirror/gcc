package Tagged_Type_Pkg is
      
   type TT is tagged limited record
      Self : access TT'Class := TT'Unchecked_Access;
   end record;
         
   function Pass_TT_Access (Obj : access TT'Class) return access TT'Class;
   
end Tagged_Type_Pkg;
