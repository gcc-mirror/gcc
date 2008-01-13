-- { dg-do run }

procedure Slice4 is

   type Varray is array (1 .. 1) of Natural;  -- SImode

   type Rec is record
      Values : Varray;
   end record;

   type Sample is record
      Maybe  : Boolean;
      R : Rec;
   end record;
   pragma Pack (Sample);

   function Match (X, Y: Sample; Length : Positive) return Boolean is
   begin
      return X.R.Values (1 .. Length) = Y.R.Values (1 .. Length);
   end;

   X, Y : Sample := (Maybe => True, R => (Values => (1 => 1)));
begin
   X.Maybe := False;
   if not Match (X, Y, 1) then
      raise Program_Error;
   end if;
end;
