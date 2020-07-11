with Interfaces;

package Aggr30 is

   type Data_Type is array (1 .. 4) of Interfaces.Unsigned_8;

   type Padding_Type is array (5 .. 4096) of Interfaces.Unsigned_8;

   type Rec is record
      Data    : Data_Type;
      Padding : Padding_Type;
   end record;

   procedure Init;

   procedure Init_Volatile;

private

   Instance : Rec;

   Instance_Volatile : Rec;
   pragma Volatile (Instance_Volatile);

end Aggr30;
