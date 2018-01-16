package BIP_Overlay
 with SPARK_Mode
is
   type X (<>) is limited private;

   pragma Warnings (gnatprove, Off,
     "volatile function ""Init"" has no volatile effects",
      reason => "Init is a pure function but returns a volatile type.");
   function Init return X
   with
      Volatile_Function;

private
   type A is limited record
      E : Integer;
   end record
      with
      Volatile;
      -- and Async_Readers when implemented;

   type X is limited new A;
end BIP_Overlay;
