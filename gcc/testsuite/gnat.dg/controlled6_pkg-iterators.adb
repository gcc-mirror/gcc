package body Controlled6_Pkg.Iterators is

   function Find return Iterator_Type is
      Iterator : Iterator_Type;
   begin
      return Iterator;
   end Find;

   function Current (Iterator : in Iterator_Type) return T is begin
      return Iterator.Current.Item;
   end Current;

   procedure Find_Next (Iterator : in out Iterator_Type) is begin
      Iterator.Current := null;
   end Find_Next;

   function Is_Null (Iterator : in Iterator_Type) return Boolean is begin
      return Iterator.Current = null;
   end Is_Null;

end Controlled6_Pkg.Iterators;
