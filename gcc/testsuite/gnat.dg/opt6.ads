package Opt6 is

   type String_Access is access all String;
   type String_List is array (Positive range <>) of String_Access;
   type String_List_Access is access all String_List;

   type Command_Line_Iterator is record
      Params   : String_List_Access;
      Current  : Natural;
   end record;

   function Current_Parameter (Iter : Command_Line_Iterator) return String;

end Opt6;
