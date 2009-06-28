package Namet is

  Hash_Num : constant Integer := 2**12;

  subtype Hash_Index_Type is Integer range 0 .. Hash_Num - 1;

  Name_Buffer : String (1 .. 16*1024);

  Name_Len : Natural;

end Namet;
