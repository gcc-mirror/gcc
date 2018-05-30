--  { dg-do run }

with Text_IO; use Text_IO;
with TLS1_Pkg; use TLS1_Pkg;

procedure TLS1 is
  Result : Integer;

  task type T is
     entry Change (Inc : Integer);
     entry Sum (Result : out Integer);
  end T;

  task body T is
  begin
      accept Change (Inc : Integer) do
         for I in My_Array.data'range loop
           My_Array.Data (I).Point := Inc;
         end loop;
      end;

      accept Sum (Result : out Integer) do
         Result := 0;
         for I in My_Array.data'range loop
           Result := Result + My_Array.Data (I).Point;
         end loop;
      end;
   end T;

   Gang : array (1..10) of T;

begin
   for J in Gang'range loop
      Gang (J).Change (J);
   end loop;

   -- Verify the contents of each local thread storage.

   for J in Gang'range loop
      Gang (J).Sum (Result);
      pragma Assert (Result = J * 500);
   end loop;

 --  Verify that original data is unaffected.

   for J in My_Array.Data'range loop
      Result := Result + My_Array.Data (J).Point;
   end loop;

   pragma Assert (Result = 500);
end TLS1;
