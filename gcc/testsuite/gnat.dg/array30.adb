--  { dg-do run }

with Ada.Text_IO;

procedure Array30 is

   package P is
      type T is tagged record
         value : Integer := 123;
      end record;

      type Ar is array (1..10) of T;
      function F (Obj : T) return Ar;
      function F2 (Obj : T) return T;
   end P;
   use P;

   package body P is
      function F (Obj : T) return Ar is
      begin
         return (others => <>);
      end;

      function F2 (Obj : T) return T is
      begin
         return (value => -111);
      end F2;
  end P;

  Thing : T;
begin
  if Thing.F (4).Value /= 0 then
     if Thing.F (5).Value /= 123 then
        raise Program_Error;
     end if;
     if Thing.F (5).F2.Value /= -111 then
        raise Program_Error;
     end if;
  end if;
end;
