pragma Restrictions (No_Exception_Propagation);

package Warn31 is

   type U16 is mod 2 ** 16;
   type U32 is mod 2 ** 32;

   type Pair is record
      X, Y : U16;
   end record;
   for Pair'Alignment use U32'Alignment;

   Blob : array (1 .. 2) of Pair;

   Sum : array (1 .. 2) of U32;
   for Sum'Address use Blob'Address;

   procedure Dummy;

end Warn31;