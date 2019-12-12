--  { dg-do compile }

pragma Spark_Mode (On);

with Predicate8_Pkg;

procedure Predicate8 is
   package Ring_Buffer is new Predicate8_Pkg (Element_Type => Integer);
   use Ring_Buffer;

   X : Ring_Buffer_Type (4);

begin
   Put (X, 1);
end Predicate8;
