pragma Restrictions (No_Implicit_Loops);

package TLS1_Pkg is
   Type My_Record_Type is record
      Date : long_float;
      Point : Integer;
   end record;

   type Nb_Type is range 0 .. 500;
   subtype Index_Type is Nb_Type range 1 .. 500;

   type My_Array_Type is array (Index_Type) of My_Record_Type;

   type My_Pseudo_Box_Type is record
      Nb : Nb_Type;
      Data : My_Array_Type;
   End record;

   My_Array : My_Pseudo_Box_Type := (Nb => 10,
     Data => (others => (Date => 3.0, Point => 1)));
   pragma Thread_Local_Storage (My_Array);

end TLS1_Pkg;
