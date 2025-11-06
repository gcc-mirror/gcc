with Ada.Text_IO;

package Prefix3_Pkg is

  type Logging is tagged record
    Output : Ada.Text_IO.File_Access;
  end record;

  procedure Log (Handler : Logging; Msg : String);

  type Logging_Class is access all Logging'Class;

  function Handler return Logging_Class;
  procedure Handler (To : Logging_Class);

end Prefix3_Pkg;
