-- { dg-do run }
-- { dg-options "-gnata" }

with Ada.Strings.Hash;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure Containers2 is
   --  Check that Cursors of the hashed containers follow the correct
   --  predefined equality rules - that two Cursors to the same element
   --  are equal, one one is obtained through, for example, iteration,
   --  and the other is obtained through a search

   subtype Definite_Name is String (1 .. 5);

   type Named_Item is
      record
         Name : Definite_Name;
         Item : Integer := 0;
      end record;


   function Equivalent_Item (Left, Right: Named_Item) return Boolean
   is (Left.Name = Right.Name);

   function DI_Hash (Item: Named_Item) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Item.Name));

   package HS is new Ada.Containers.Hashed_Sets
     (Element_Type        => Named_Item,
      Hash                => DI_Hash,
      Equivalent_Elements => Equivalent_Item);

   package IHS is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Named_Item,
      Hash                => DI_Hash,
      Equivalent_Elements => Equivalent_Item);

   package HM is new Ada.Containers.Hashed_Maps
     (Key_Type        => Definite_Name,
      Element_Type    => Integer,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   package IHM is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Definite_Name,
      Element_Type    => Integer,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Item_Data : constant array (1 .. 5) of Named_Item
     := ((Name => "ABCDE", others => <>),
         (Name => "FGHIJ", others => <>),
         (Name => "KLMNO", others => <>),
         (Name => "PQRST", others => <>),
         (Name => "UVWXY", others => <>));

   use type HS.Cursor;
   use type IHS.Cursor;
   use type HM.Cursor;
   use type IHM.Cursor;

   type HS_Cursor_Vec  is array (Item_Data'Range) of HS.Cursor;
   type IHS_Cursor_Vec is array (Item_Data'Range) of IHS.Cursor;
   type HM_Cursor_Vec  is array (Item_Data'Range) of HM.Cursor;
   type IHM_Cursor_Vec is array (Item_Data'Range) of IHM.Cursor;

   HSC  : HS.Set;
   IHSC : IHS.Set;
   HMC  : HM.Map;
   IHMC : IHM.Map;

   HS_Create_Cursors  : HS_Cursor_Vec;
   IHS_Create_Cursors : IHS_Cursor_Vec;
   HM_Create_Cursors  : HM_Cursor_Vec;
   IHM_Create_Cursors : IHM_Cursor_Vec;

   HS_Index  : HS.Cursor;
   IHS_Index : IHS.Cursor;
   HM_Index  : HM.Cursor;
   IHM_Index : IHM.Cursor;

   HS_Find  : HS.Cursor;
   IHS_Find : IHS.Cursor;
   HM_Find  : HM.Cursor;
   IHM_Find : IHM.Cursor;


   Inserted : Boolean;

begin

   for I in Item_Data'Range loop
      HSC.Insert (New_Item => Item_Data(I),
                  Position => HS_Create_Cursors(I),
                  Inserted => Inserted);

      pragma Assert (Inserted);


      IHSC.Insert (New_Item => Item_Data(I),
                   Position => IHS_Create_Cursors(I),
                   Inserted => Inserted);

      pragma Assert (Inserted);

      HMC.Insert (New_Item => Item_Data(I).Item,
                  Key      => Item_Data(I).Name,
                  Position => HM_Create_Cursors(I),
                  Inserted => Inserted);

      pragma Assert (Inserted);

      IHMC.Insert (New_Item => Item_Data(I).Item,
                   Key      => Item_Data(I).Name,
                   Position => IHM_Create_Cursors(I),
                   Inserted => Inserted);

      pragma Assert (Inserted);

   end loop;

   HS_Index  := HSC.First;
   IHS_Index := IHSC.First;
   HM_Index  := HMC.First;
   IHM_Index := IHMC.First;

   for I in Item_Data'Range loop
      pragma Assert (HS.Has_Element  (HS_Index));
      pragma Assert (IHS.Has_Element (IHS_Index));
      pragma Assert (HM.Has_Element  (HM_Index));
      pragma Assert (IHM.Has_Element (IHM_Index));

      HS_Find := HSC.Find (Item_Data(I));
      pragma Assert (HS_Create_Cursors(I) = HS_Index);
      pragma Assert (HS_Find = HS_Index);

      IHS_Find := IHSC.Find (Item_Data(I));
      pragma Assert (IHS_Create_Cursors(I) = IHS_Index);
      pragma Assert (IHS_Find = IHS_Index);

      HM_Find := HMC.Find (Item_Data(I).Name);
      pragma Assert (HM_Create_Cursors(I) = HM_Index);
      pragma Assert (HM_Find = HM_Index);

      IHM_Find := IHMC.Find (Item_Data(I).Name);
      pragma Assert (IHM_Create_Cursors(I) = IHM_Index);
      pragma Assert (IHM_Find = IHM_Index);

      HS.Next  (HS_Index);
      IHS.Next (IHS_Index);
      HM.Next  (HM_Index);
      IHM.Next (IHM_Index);
   end loop;

end;
