package volatile1 is

   type Command is (Nothing, Get);

   type Data is
      record
         Time : Duration;
      end record;

   type Data_Array is array (Integer range <>) of Data;

   type Command_Data (Kind : Command; Length : Integer) is
      record
         case Kind is
            when Nothing =>
               null;
            when Get =>
               Data : Data_Array (1 .. Length);
         end case;
      end record;

end;
