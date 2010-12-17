-- { dg-do run }

procedure Sizetype4 is

   type Float_Array is array (Integer range <>) of Float;
   NoFloats : Float_Array (1 .. 0);

   procedure Q (Results : Float_Array := NoFloats) is

      type Reply_Msg is
         record
            Request_Id : Integer;
            Status     : Integer;
            Data       : Float_Array (Results'Range);
         end record;

   begin
      if Reply_Msg'Size /= 64 then
        raise Program_Error;
      end if;
   end;

begin
   Q;
end;
