with volatile1; use volatile1;

package volatile2 is
   
   type PData_Array is access Data_Array;
   
   type Result_Desc is
      record
         Data : PData_Array;
      end record;
   
   type Result is access Result_Desc;
   
   procedure Copy;

end volatile2;
