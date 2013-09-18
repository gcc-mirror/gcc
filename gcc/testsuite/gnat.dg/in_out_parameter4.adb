-- { dg-do run }
-- { dg-options "-gnat12 -gnatVa" }

procedure In_Out_Parameter4 is

   type Enum is (E_Undetermined, E_Down, E_Up);
   subtype Status_T is Enum range E_Down .. E_Up;

   function Recurse (Val : in out Integer) return Status_T is

     Result : Status_T;

     procedure Dummy (I : in out Integer) is begin null; end;

   begin
     if Val > 500 then
       Val := Val - 1;
       Result := Recurse (Val);
       return Result;
     else
       return E_UP;
     end if;
   end;

   Val : Integer := 501;
   S : Status_T;

begin
   S := Recurse (Val);
end;
