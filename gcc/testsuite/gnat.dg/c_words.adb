--  { dg-do compile }

package body C_Words is
   
   function New_Word (Str : String) return Word is
   begin
      return (Str'Length, Str);
   end New_Word;
   
   function New_Word (Str : String) return C_Word is
   begin
      return (Str'Length, Str);
   end New_Word;
end C_Words;
