with Ada.Finalization;

generic
package Equal4_Smart_Pointers is
   type Pointer is private;

private
   type Pointer is new Ada.Finalization.Controlled with record
      Data : Integer;
   end record;
end Equal4_Smart_Pointers;
