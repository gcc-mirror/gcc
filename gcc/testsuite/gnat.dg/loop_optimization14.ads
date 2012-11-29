package Loop_Optimization14 is

   type Rec is record
      A : Boolean;
      pragma Atomic (A);

      B : Boolean;

   end record;

   procedure Finalize_Pool (Pool : in out Rec);

end Loop_Optimization14;
