-- { dg-do run }

procedure Test_Self_Ref is
   type T2;
   type T2_Ref is access all T2; 

   function F (X: T2_Ref) return Integer;

   type T2 is limited record
      Int1 : Integer := F (T2'Unchecked_Access);
      Int2 : Integer := F (T2'Unrestricted_Access);
   end record; 

   Counter : Integer := 2;

   function F (X: T2_Ref) return Integer is
   begin   
      Counter := Counter * 5;
      return Counter;
   end F;  

   Obj1 : T2_Ref := new T2'(10,20);
   Obj2 : T2_Ref := new T2; 
   Obj3 : T2_Ref := new T2'(others => <>); 

begin   
  if Obj1.Int1 /= 10 or else Obj1.Int2 /= 20 then    
     raise Program_Error;
  end if; 
  if Obj2.Int1 /= 10 or else Obj2.Int2 /= 50 then    
     raise Program_Error;
  end if; 
  if Obj3.Int1 /= 250 or else Obj3.Int2 /= 1250 then    
     raise Program_Error;
  end if; 
end Test_Self_Ref;
