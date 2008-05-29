with Ada.Streams; use Ada.Streams;
package abstract1 is
   type T is abstract tagged limited null record;
   function Input (Stream : not null access Root_Stream_Type'Class) return T
     is abstract;
   
   function New_T (Stream : not null access Root_Stream_Type'Class)
     return T'Class;
   
   type IT is limited new T with record
     I : Integer;
   end record;
   function Input (Stream : not null access Root_Stream_Type'Class) return IT;
   
   type FT is limited new T with record
     F : Float;
   end record;
   function Input (Stream : not null access Root_Stream_Type'Class) return FT;
end abstract1;
