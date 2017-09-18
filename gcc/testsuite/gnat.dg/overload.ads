package Overload is

   type Rec1 is record
      Data : Integer;
   end record;
   type Ptr1 is access all Rec1;

   type Rec2 is record
      Data : aliased Rec1;
   end record;

   type Ptr2 is access Rec2;

   function Get (I : Integer) return Ptr1;

   function Get (I : Integer) return Ptr2;

   function F (I : Integer) return Ptr1;
     
end Overload;
