-- { dg-do compile }

procedure Varsize1 (Nbytes : Natural) is

   type Message_T (Length : Natural) is record
      case Length is
         when 0 => null;
         when others => Id : Natural;
      end case;
   end record;

   type Local_Message_T is new Message_T (Nbytes);

   function One_message return Local_Message_T is
      M : Local_Message_T;
   begin
      if M.Length > 0 then
         M.Id := 1;
      end if;
      return M;
   end;

   procedure Process (X : Local_Message_T) is begin null; end;

begin
   Process (One_Message);
end;
