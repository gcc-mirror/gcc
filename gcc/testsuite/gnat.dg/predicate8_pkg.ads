pragma Spark_Mode (On);

generic
  type Element_Type is private;

package Predicate8_Pkg is
   pragma Annotate (GNATprove, Terminating, Predicate8_Pkg);

   subtype Small_Natural  is Natural range 0 .. Natural'Last / 2;
   subtype Small_Positive is Natural range 1 .. Natural'Last / 2;

   type Element_Array_Type is array (Small_Positive range <>) of Element_Type;

   type Ring_Buffer_Type (Max_Size : Small_Positive) is private
     with Default_Initial_Condition => Empty (Ring_Buffer_Type);

   function Empty
     (Buffer : in Ring_Buffer_Type) return Boolean;

   function Full
     (Buffer : in Ring_Buffer_Type) return Boolean;

   function Size
     (Buffer : in Ring_Buffer_Type) return Natural;

   function Free
     (Buffer : in Ring_Buffer_Type) return Natural;

   function First
     (Buffer : in Ring_Buffer_Type) return Element_Type
   with
     Pre => not Empty (Buffer);

   function Last
     (Buffer : in Ring_Buffer_Type) return Element_Type
   with
     Pre => not Empty (Buffer);

   procedure Get
     (Buffer   : in out Ring_Buffer_Type;
      Element  :    out Element_Type)
   with
     Pre   => not Empty (Buffer) and
              Size (Buffer) >= 1,
     Post  => not Full (Buffer) and then
              Element = First (Buffer'Old) and then
              Size (Buffer) = Size (Buffer'Old) - 1;

   procedure Put
     (Buffer   : in out Ring_Buffer_Type;
      Element  : in     Element_Type)
   with
     Pre   => not Full (Buffer),
     Post  => not Empty (Buffer) and then
              Last (Buffer) = Element and then
              Size (Buffer) = Size (Buffer'Old) + 1;

   procedure Clear
     (Buffer : in out Ring_Buffer_Type)
   with
     Post => Empty (Buffer) and then
             not Full (Buffer) and then
             Size (Buffer) = 0;

private
   type Ring_Buffer_Type (Max_Size : Small_Positive) is record
      Count : Small_Natural  := 0;
      Head  : Small_Positive := 1;
      Tail  : Small_Positive := Max_Size;
      Items : Element_Array_Type (1 .. Max_Size);
   end record
     with Dynamic_Predicate =>
       (Max_Size <= Small_Positive'Last and
        Count    <= Max_Size and
        Head     <= Max_Size and
        Tail     <= Max_Size and
        ((Count = 0 and Tail = Max_Size and Head = 1) or
         (Count = Max_Size + Tail - Head + 1) or
         (Count = Tail - Head + 1)));

end Predicate8_Pkg;
