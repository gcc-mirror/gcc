--  { dg-do compile }

procedure Warn23 is

   type Enum_Type is (A, B, C);

   function Poll (E : out Enum_Type) return Boolean
     with Convention => Ada,
          Import => True;

   E : Enum_Type;

begin
   while Poll (E) loop
      null;
   end loop;
end;
