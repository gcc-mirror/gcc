with Opt28_Pkg; use Opt28_Pkg;

package body Opt28 is

  function Full_Filename (Filename : String) return String is
    Path : constant String := "PATH";
    Posix_Path : constant Posix_String := To_Posix (Path);
  begin

    declare
      M : constant Posix_String := Value_Of (Posix_Path);
      N : constant Posix_String (1 .. M'Length) := M;
      Var : constant String := To_String (Str => N);
      Start_Pos : Natural := 1;
      End_Pos   : Natural := 1;
    begin
      while Start_Pos <= Var'Length loop
        End_Pos := Position (Var (Start_Pos .. Var'Length));

        if Is_File (To_Posix (Var (Start_Pos .. End_Pos - 1) & Filename)) then
          return Var (Start_Pos .. End_Pos - 1) & Filename;
        else
          Start_Pos := End_Pos + 1;
        end if;
      end loop;
    end;

    return "";
  end;

end Opt28;
