-- { dg-do compile }
-- { dg-options "-O -fdump-tree-optimized" }

procedure Loop_Optimization20 is

  type Array_T is array (Positive range <>) of Integer;
  type Obj_T (Length : Natural) is
    record
      Elements : Array_T (1 .. Length);
    end record;

  type T is access Obj_T;

  function Is_Null (S1 : Obj_T) return Boolean;
  pragma No_Inline (Is_Null);

  function Is_Null (S1 : Obj_T) return Boolean is
  begin
    for I in 1 .. S1.Length loop
      if S1.Elements (I) /= 0 then
        return False;
      end if;
    end loop;
    return True;
  end;

  A : T := new Obj_T'(Length => 10, Elements => (others => 0));

begin
  if not Is_Null (A.all) then
    raise Program_Error;
  end if;
end;

-- { dg-final { scan-tree-dump-not "Index_Check" "optimized" } }
