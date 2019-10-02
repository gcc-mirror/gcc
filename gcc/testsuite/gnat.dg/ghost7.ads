pragma Restrictions (No_Exception_Propagation);

package Ghost7 is
   type Word64 is mod 2**64;
   type My_Array_Type is array (Word64) of Boolean;
   My_Array : My_Array_Type with Ghost;
   procedure Dummy;
end Ghost7;