package Pack15 is

  type Flags is array (1..2) of Boolean;
  for Flags'Component_Size use 1;

  type Messages is record
    Status_Flags : Flags;
  end record;

  for Messages use record
    Status_Flags at 0 range 1 .. 2;
  end record;

  O : Messages;

  Buffer : Integer;
  Status_Flags : Flags;
  for Status_Flags'Address use Buffer'Address;

  procedure Transfer;

end Pack15;
