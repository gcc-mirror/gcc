-- { dg-do compile }
package body Enclosing_Record_Reference is

    R: aliased T;

    function F1 (x: integer) return T         is begin return R; end;
    function F2 (x: T) return integer         is begin return 0; end;
    function F3 (x: T) return T               is begin return R; end;
    function F4 (x: integer) return access T  is begin return R'access; end;
    function F5 (x: access T) return integer  is begin return 0; end;
    function F6 (x: access T) return access T is begin return R'access; end;
    function F7 (x: T) return access T        is begin return R'access; end;
    function F8 (x: access T) return T        is begin return R; end;

begin
    R.F1 := F1'Access;
    R.F2 := F2'Access;
    R.F3 := F3'Access;
    R.F4 := F4'Access;
    R.F5 := F5'Access;
    R.F6 := F6'Access;
    R.F7 := F7'Access;
    R.F8 := F8'Access;
end Enclosing_Record_Reference;
