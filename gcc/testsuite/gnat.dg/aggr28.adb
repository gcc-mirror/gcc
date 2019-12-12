--  { dg-do run }

procedure Aggr28 is

  Count : Natural := 0;

  function Get (S: String) return String is
  begin
    Count := Count + 1;
    return S;
  end;

  Max_Error_Length : constant := 8;
  subtype Error_Type is String (1 .. Max_Error_Length);

  type Rec is record
    Text : Error_Type;
  end record;

  type Arr is array (1 .. 16) of Rec;

  Table : constant Arr :=
    (3 => (Text => Get ("INVALID ")), others => (Text => Get ("OTHERS  ")));

begin
  if Count /= Table'Length then
    raise Program_Error;
  end if;
end;