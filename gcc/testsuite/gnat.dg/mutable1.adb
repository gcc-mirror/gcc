-- { dg-do run }

procedure mutable1 is
            
   type Object (Valid : Boolean := False) is record
      case Valid is
         when True  => Stamp : Natural;
         when False => null;
      end case;
   end record;
      
   function Dummy_Object (Should_Be_There : Boolean) Return Object is
   begin 
      if not Should_Be_There then
         raise Program_Error;
      end if;
      return Object'(Valid => False);
   end;

   procedure Check (Create_Dummy : Boolean) is
      B : Boolean;
   begin
      B := Create_Dummy and then Dummy_Object (Create_Dummy).Valid;
   end;

begin
   Check (Create_Dummy => False); 
   Check (Create_Dummy => True);
end;
