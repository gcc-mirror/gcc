
package body Nested_Subtype_Byref is

   type Data (Stamped : Boolean) is record
      case Stamped is
         when True   => Valid : Boolean;
         when others => null;
      end case;
   end record;

   type Message is record
      F : Integer := 1;
      D : Data (Stamped => True);
   end record;

   procedure Check  is
      M : Message;
   begin
      M.D.Valid := True;
   end;

end;

