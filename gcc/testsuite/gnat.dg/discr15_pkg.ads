package Discr15_Pkg is

   type Moment is new Positive;

   type Multi_Moment_History is array (Natural range <>, Moment range <>) of Float;

   type Rec_Multi_Moment_History (Len : Natural; Size : Moment) is
   record
      Moments : Multi_Moment_History(0..Len, 1..Size);
      Last    : Natural;
   end record;

   function Sub_History_Of (History : Rec_Multi_Moment_History)
      return Rec_Multi_Moment_History;

end Discr15_Pkg;
