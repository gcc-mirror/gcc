with Ada.Text_IO;

package Renaming19_Pkg is

  type Logging is tagged record
    Output : Ada.Text_IO.File_Access;
  end record;

  procedure Log (Handler : Logging; Msg : String);
  procedure Log (Handler : Logging; Msg : String; Err : Natural);

  type Full_Logging is new Logging with null record;

  procedure Log (Handler : Full_Logging; Msg : String);

end Renaming19_Pkg;
