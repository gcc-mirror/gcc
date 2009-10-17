generic

   Line_Length : Natural;
   Max_Lines   : Natural;

package Slice8_Pkg2 is

   Subtype Index      is Natural Range 0..Line_length;
   Subtype Line_Count is Natural Range 0..Max_Lines;

   Type Line (Size : Index := 0) is
   Record
      Data : String (1..Size);
   End Record;

   Type Lines is Array (Line_Count Range <>) of Line;

   Type Paragraph (Size : Line_Count) is
   Record
      Data : Lines (1..Size);
   End Record;

end Slice8_Pkg2;
