generic

   UB1 : Natural;

   UB2 : Natural;

package Array25_Pkg is

   type Arr1 is array (1 .. UB1) of Integer;

   type Rec is record
      Data : Arr1;
   end record;

   type Arr2  is array (1 .. UB2) of Rec;

   procedure Get (A : out Arr2);

end Array25_Pkg;
