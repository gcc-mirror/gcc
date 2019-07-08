pragma Spark_Mode (On);

package body Predicate8_Pkg is
   function Empty
     (Buffer : in Ring_Buffer_Type) return Boolean
   is (Size (Buffer) = 0);

   function Full
     (Buffer : in Ring_Buffer_Type) return Boolean
   is (Size (Buffer) = Buffer.Max_Size);

   function Size
     (Buffer : in Ring_Buffer_Type) return Natural
   is (Buffer.Count);

   function Free
     (Buffer : in Ring_Buffer_Type) return Natural
   is (Buffer.Max_Size - Size (Buffer));

   function First
     (Buffer : in Ring_Buffer_Type) return Element_Type
   is (Buffer.Items (Buffer.Head));

   function Last
     (Buffer : in Ring_Buffer_Type) return Element_Type
   is (Buffer.Items (Buffer.Tail));

   procedure Get
     (Buffer   : in out Ring_Buffer_Type;
      Element  :    out Element_Type)
   is
   begin
      Element := Buffer.Items (Buffer.Head);
      Buffer  :=
        Buffer'Update
          (Head  =>
            (if Buffer.Head = Buffer.Max_Size then 1 else Buffer.Head + 1),
           Count => Buffer.Count - 1);
   end Get;

   procedure Put
     (Buffer   : in out Ring_Buffer_Type;
      Element  : in     Element_Type)
   is
    begin
      if Buffer.Tail = Buffer.Max_Size then
         Buffer.Tail := 1;
      else
         Buffer.Tail := Buffer.Tail + 1;
      end if;

      Buffer.Items (Buffer.Tail) := Element;
      Buffer.Count := Buffer.Count + 1;
   end Put;

   procedure Clear
     (Buffer : in out Ring_Buffer_Type)
   is
   begin
      Buffer.Head  := 1;
      Buffer.Tail  := Buffer.Max_Size;
      Buffer.Count := 0;
   end Clear;
end Predicate8_Pkg;
