-- { dg-do compile }
-- { dg-options "-g" }

with Debug2_Pkg; use Debug2_Pkg;

package body Debug2 is

    procedure Proc is

        function F return String_List_Ptr is
        begin
            return new String_List'(Singleton);
        end;

        A : String_List_Ptr := F;

    begin
        null;
    end;

    function Get return Integer is
    begin
        return 0;
    end;

    Failed : exception;

    A: String_Ptr;

begin

    declare
        Server_Args : Integer;
    begin
        Server_Args := Get;
    exception
        when X : Failed => A := To_Heap;
    end;

end Debug2;
