-- { dg-do run }
-- { dg-options "-O2 -gnatp -fno-omit-frame-pointer" }

procedure Opt76 is

   type Integer_Access is access Integer;
   type Registry_Array is array (Natural range <>) of Integer_Access;

   procedure Nested (Input, Parser : Integer; A, B : Boolean) is

      Index : Registry_Array (1 .. 1024);
      Not_B : constant Boolean := not B;

      procedure Inner (Input : Integer) is
      begin
         if Input /= 1 then
            raise Program_Error;
         end if;

         if Parser = 128 and then A and then Not_B then
            Inner (Input);
            Index (Index'First) := null;
         end if;
      end;

   begin
      Inner (Input);
   end;

   Input : Integer := 1 with Volatile;
   Parser : Integer := 2 with Volatile;
      
begin
   Nested (Input, Parser, False, True);
   Nested (Input, Parser, True, False);
end;
