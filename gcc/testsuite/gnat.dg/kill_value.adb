-- { dg-do run }

procedure kill_value is
   type Struct;
   type Pstruct is access all Struct;
   
   type Struct is record Next : Pstruct; end record;
   
   Vap : Pstruct := new Struct;

begin
   for J in 1 .. 10 loop
      if Vap /= null then
         while Vap /= null
         loop
            Vap := Vap.Next;
         end loop;
      end if;
   end loop;
end;
