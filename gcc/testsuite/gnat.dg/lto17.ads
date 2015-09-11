package Lto17 is

   type Chunk_List_Element;
   type Chunk_List is access Chunk_List_Element;

   type Arr is array (Natural range <>) of Integer;

   type Chunk(Size : Natural) is record
      Data  : Arr(1 .. Size);
      Where : Natural;
   end record;

   type Chunk_List_Element(Size : Natural) is record
      Chnk : Chunk(Size);
      Link : Chunk_List;
   end record;

   function To_Chunk_List(C : Chunk) return Chunk_List;

end Lto17;
