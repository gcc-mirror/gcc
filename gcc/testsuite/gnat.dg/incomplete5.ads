with Incomplete5_Pkg;

package Incomplete5 is

   type Rec1 is private;

   type Rec2 is private;

   package My_G is new Incomplete5_Pkg (Rec1);

   use My_G;

   function Get (O: Base_Object) return Integer;

private

   type Rec1 is record
      I : Integer;
   end record;

   type Rec2 is record
      A : Access_Type;
   end record;

end Incomplete5;
