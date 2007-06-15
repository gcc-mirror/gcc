package C_Words is
   type Comparable is limited interface;
   
   type Word (<>) is tagged private;
   function New_Word (Str : String) return Word;
   
   type C_Word (<>) is new Word and Comparable with private;
   function New_Word (Str : String) return C_Word;

private
   type Word (Length : Natural) is tagged record
      Str : String (1 .. Length) := (others => ' ');
   end record;
   
   type C_Word is new Word and Comparable with null record;
end C_Words;
