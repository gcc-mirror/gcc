with Ada.Streams, Ada.Tags;
package gen_disp is
   type Root_Type is tagged null record;
   
   function Root_Type_Class_Input
     (S    : not null access Ada.Streams.Root_Stream_Type'Class)
      return Root_Type'Class;
      
   for Root_Type'Class'Input use Root_Type_Class_Input;
end gen_disp;
