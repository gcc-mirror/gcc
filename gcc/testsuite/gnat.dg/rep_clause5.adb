-- { dg-do compile }
-- { dg-options "-O" }

package body Rep_Clause5 is

    function To_LNumber(S : String) return LNumber_Type is
        V :  VString;
        LV : Long_Type;
        LN : LNumber_Type;
    begin
        LV := To_Long(V, 10);
        LN := LNumber_Type(LV);
        return LN;
    end;

    procedure Merge_Numbered(LNodes : in out LNodes_Ptr) is
        T1  : Token_Type;
        LNO : LNumber_Type;
    begin
        for X in LNodes.all'Range loop
            T1 := LNodes(X).Line(0);
            if T1.Token /= LEX_LF then
                declare
                    S : String := Element(T1.SID);
                begin
                    begin
                        LNO := To_LNumber(S);
                    exception
                        when Bad_Number =>
                            LNO := 0;
                        when Too_Large =>
                            LNO := 0;
                    end;
                end;
            end if;
        end loop;
    end;

end Rep_Clause5;
