-- { dg-do compile }

procedure Discr38 is

   type Enum is (OK,
                 NOT_CONNECTED,
                 DISCONNECTED,
                 REQUEST_Q_EMPTY,
                 SERVER_UNAVAILABLE,
                 BUFFER_TOO_SMALL,
                 NO_FREE_SLOT,
                 RAISE_EXCEPTION,
                 REQUEST_CANCELLED,
                 REQUEST_IN_PROGRESS,
                 SERVER_BUSY,
                 BLOCK_ACKNOWLEDGE);

   type R (Status : Enum := OK) is record
      Status_Block : Integer;
      case Status is
      when RAISE_EXCEPTION =>
         I : Integer;
      when OK =>
         Length : Natural;
         Data   : Integer;
      when others =>
         null;
      end case;
   end record;
   for R use record
      Status        at  0 range 0 .. 7;
      Status_Block  at  4 range 0 .. 31;
      Length        at  8 range 0 .. 31;
   end record;

   Nil : constant R := (OK, 1, 0, 1);

begin
   null;
end;
