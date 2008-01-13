-- { dg-do run }

procedure Slice3 is

   type Varray is array (1 .. 1) of Natural;  -- SImode

   type Sample is record
      Maybe  : Boolean;
      Values : Varray;
   end record;
   pragma Pack (Sample);

   function Match (X, Y: Sample; Length : Positive) return Boolean is
   begin
      return X.Values (1 .. Length) = Y.Values (1 .. Length);
   end;

   X, Y : Sample := (Maybe => True, Values => (1 => 1));
begin
   X.Maybe := False;
   if not Match (X, Y, 1) then
      raise Program_Error;
   end if;
end;
