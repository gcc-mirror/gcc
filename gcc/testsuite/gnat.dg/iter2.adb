--  { dg-do compile }
--  { dg-options "-gnatd.F -gnatws" }

package body Iter2
   with SPARK_Mode
is
   function To_String (Name : String) return String
   is
      procedure Append (Result : in out String;
                        Data   :        String)
        with Inline_Always;
      procedure Append (Result : in out String;
                        Data   :        String)
      is
      begin
         for C of Data
         loop
            Result (1) := C;
         end loop;
      end Append;

      Result : String (1 .. 3);
   begin
      Append (Result, "</" & Name & ">");
      return Result;
   end To_String;

end Iter2;
