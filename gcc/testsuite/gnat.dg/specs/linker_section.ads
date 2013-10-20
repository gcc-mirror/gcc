package Linker_Section is
   Data1 : constant String := "12345678901234567";
   pragma Linker_Section (Entity  => Data1,
                          Section => ".eeprom");
   type EEPROM_String is new String;
   pragma Linker_Section (Entity  => EEPROM_String,
                          Section => ".eeprom");
   Data2 : constant EEPROM_String := "12345678901234567";
   package Inner is end;
   pragma Linker_Section (Entity  => Inner,        -- { dg-error "objects" }
                          Section => ".eeprom");
end Linker_Section;

