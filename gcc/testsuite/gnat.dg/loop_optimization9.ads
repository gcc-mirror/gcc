with GNAT.Strings; use GNAT.Strings;

package Loop_Optimization9 is

   type File_Descriptor is new Integer;

   procedure Create_Temp_File_Internal
     (FD   : out File_Descriptor;
      Name : out String_Access);

   subtype Argument_List is String_List;

   subtype Argument_List_Access is String_List_Access;

   function Argument_String_To_List
     (Arg_String : String) return Argument_List_Access;

end Loop_Optimization9;
