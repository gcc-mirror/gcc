with Concat5_Pkg2; use Concat5_Pkg2;

package body Concat5_Pkg1 is

  procedure Make_Failed (S : String);
  pragma No_Inline (Make_Failed);

  procedure Make_Failed (S : String) is
  begin
    Compare (S);
  end;

  procedure Scan (S : String) is
  begin
    Make_Failed ("option " & S & " should start with '--'");
  end;

end Concat5_Pkg1;
