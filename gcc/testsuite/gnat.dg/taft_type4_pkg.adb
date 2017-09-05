with Unchecked_Deallocation;

package body Taft_Type4_Pkg is

  type Obj_T is null record;

  procedure Unchecked_Free is new Unchecked_Deallocation (Obj_T, T);

  procedure Proc (L : in out T) is
  begin
    Unchecked_Free (L);
  end;

end Taft_Type4_Pkg;
