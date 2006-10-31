package body inline_scope_p is
   procedure Assert (Expr : Boolean; Str : String) is
   begin   
      if Expr then
         null;   
      end if; 
   end Assert; 
end;    
