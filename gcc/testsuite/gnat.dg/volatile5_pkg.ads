package Volatile5_Pkg is

   type Rec is record
      I : Integer;
   end record;
   pragma Volatile(Rec);

  function F return Rec;

end Volatile5_Pkg;
-- 