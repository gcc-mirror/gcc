-- { dg-do compile }
-- { dg-options "-O -fdump-tree-optimized" }

procedure Loop_Optimization19 is

  type Array_T is array (Positive range <>) of Integer;
  type Obj_T (Length : Natural) is
    record
      Elements : Array_T (1 .. Length);
    end record;

  type T is access Obj_T;

  function Equal (S1, S2 : T) return Boolean;
  pragma No_Inline (Equal);

  function Equal (S1, S2 : T) return Boolean is
  begin
    if S1.Length = S2.Length then
      for I in 1 .. S1.Length loop
        if S1.Elements (I) /= S2.Elements (I) then
          return False;
        end if;
      end loop;
     return True;
    else
      return False;
    end if;
  end Equal;

  A : T := new Obj_T (Length => 10);
  B : T := new Obj_T (Length => 20);
  C : T := new Obj_T (Length => 30);

begin
  if Equal (A, B) then
    raise Program_Error;
  else
    if Equal (B, C) then
      raise Program_Error;
    end if;
  end if;
end;

-- { dg-final { scan-tree-dump-not "Index_Check" "optimized" } }
