generic
   type Component is private;
   type List_Index is range <>;
   type List is array (List_Index range <>) of Component;
   Default_Value : Component;
 --  with function "=" (Left, Right : List) return Boolean is <>;

package Equal8_Pkg is

   pragma Pure;

   Maximum_Length : constant List_Index := List_Index'Last;

   subtype Natural_Index is List_Index'Base range 0 .. Maximum_Length;
   type Sequence (Capacity : Natural_Index) is private;
   --  from zero to Capacity.

   function Value (This : Sequence) return List;
   --  Returns the content of this sequence. The value returned is the
   --  "logical" value in that only that slice which is currently assigned
   --  is returned, as opposed to the entire physical representation.

   overriding
   function "=" (Left, Right : Sequence) return Boolean with
     Inline;

   function "=" (Left : Sequence;  Right : List) return Boolean with
     Inline;

private
   type Sequence (Capacity : Natural_Index) is record
      Current_Length : Natural_Index := 0;
      Content        : List (1 .. Capacity) := (others => Default_Value);
   end record;

   -----------
   -- Value --
   -----------

   function Value (This : Sequence) return List is
     (This.Content (1 .. This.Current_Length));

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (Left, Right : Sequence) return Boolean is
     (Value (Left) = Value (Right));

   ---------
   -- "=" --
   ---------

   function "=" (Left : Sequence;  Right : List) return Boolean is
     (Value (Left) = Right);
end Equal8_Pkg;

