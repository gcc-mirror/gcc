--  { dg-do compile }

with Ada.Containers.Formal_Hashed_Sets;

procedure Formal_Containers is
   type T is new Integer;

   function Eq (X : T; Y : T) return Boolean;

   function Hash (X : T) return Ada.Containers.Hash_Type is (0);

   package TSet is new Ada.Containers.Formal_Hashed_Sets
     (Element_Type        => T,
      Hash                => Hash,
      Equivalent_Elements => Eq);

   S : Tset.Set := TSet.Empty_Set;

   function Eq (X : T; Y : T) return Boolean is
   begin
      return TSet.Contains (S, X) or TSet.Contains (S, Y);
   end Eq;
begin null; end Formal_Containers;
