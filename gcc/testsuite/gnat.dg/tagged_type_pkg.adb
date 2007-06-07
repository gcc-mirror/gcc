package body Tagged_Type_Pkg is
      
   function Pass_TT_Access (Obj : access TT'Class) return access TT'Class is
   begin
      if Obj = null then
         return null;
      
      else
         --  The implicit conversion in the assignment to the return object
         --  must fail if Obj's actual is not a library-level object.
         
         return TT_Acc : access TT'Class := Obj do
            TT_Acc := TT_Acc.Self;
         end return;
      end if;
   end Pass_TT_Access;
   
end Tagged_Type_Pkg;
