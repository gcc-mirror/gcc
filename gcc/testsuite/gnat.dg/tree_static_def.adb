
package body TREE_STATIC_Def is

 procedure check (i : int; v : integer) is
 begin
    if i.value /= v then
      raise program_error;
    end if;
 end;
end;

