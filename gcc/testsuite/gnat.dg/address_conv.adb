--  { dg-do compile }

with System.Storage_Elements; use System.Storage_Elements;

procedure Address_Conv is

  subtype My_Address is System.Address;

  type Rec is record
    A : My_Address;
  end record;

  Addr : constant My_Address := To_Address (16#FACEFACE#);

  R : constant Rec := (A => Addr);

begin
  null;
end;
