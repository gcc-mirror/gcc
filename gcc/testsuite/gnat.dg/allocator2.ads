pragma SPARK_Mode;
package Allocator2 is
    type Nat_Array is array (Positive range <>) of Natural with
      Default_Component_Value => 0;
    type Nat_Stack (Max : Natural) is record
       Content : Nat_Array (1 .. Max);
    end record;
    type Stack_Acc is access Nat_Stack;
    type My_Rec is private;
private
    type My_Rec is record
       My_Stack : Stack_Acc := new Nat_Stack (Max => 10);
    end record;
   procedure Dummy;
end Allocator2;
