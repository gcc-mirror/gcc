--  { dg-do compile }

package body Overload is

   function Get (I : Integer) return Ptr1 is
      P : Ptr1 := null;
   begin
      return P;
   end;

   function Get (I : Integer) return Ptr2 is
      P : Ptr2 := null;
   begin
      return P;
   end;

   function F (I : Integer) return Ptr1 is
     P : Ptr1 := Get (I).Data'Access;
   begin
     return P;
   end;

end Overload;
