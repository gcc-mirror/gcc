-- C761012.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--    Check that an anonymous object is finalized with its enclosing master if
--    a transfer of control or exception occurs prior to performing its normal
--    finalization.  (Defect Report 8652/0023, as reflected in
--    Technical Corrigendum 1, RM95 7.6.1(13.1/1)).
--
-- CHANGE HISTORY:
--    29 JAN 2001   PHL   Initial version.
--     5 DEC 2001   RLB   Reformatted for ACATS.
--
--!
with Ada.Finalization;
use Ada.Finalization;
package C761012_0 is

    type Ctrl (D : Boolean) is new Controlled with
        record
            case D is
                when False =>
                    C1 : Integer;
                when True =>
                    C2 : Float;
            end case;
        end record;

    function Create return Ctrl;
    procedure Finalize (Obj : in out Ctrl);
    function Finalize_Was_Called return Boolean;

end C761012_0;

with Report;
use Report;
package body C761012_0 is

    Finalization_Flag : Boolean := False;

    function Create return Ctrl is
        Obj : Ctrl (Ident_Bool (True));
    begin
        Obj.C2 := 3.0;
        return Obj;
    end Create;

    procedure Finalize (Obj : in out Ctrl) is
    begin
        Finalization_Flag := True;
    end Finalize;

    function Finalize_Was_Called return Boolean is
    begin
        if Finalization_Flag then
            Finalization_Flag := False;
            return True;
        else
            return False;
        end if;
    end Finalize_Was_Called;

end C761012_0;

with Ada.Exceptions;
use Ada.Exceptions;
with C761012_0;
use C761012_0;
with Report;
use Report;
procedure C761012 is
begin
    Test ("C761012",
          "Check that an anonymous object is finalized with its enclosing " &
             "master if a transfer of control or exception occurs prior to " &
             "performing its normal finalization");

    Excep:
        begin

            declare
                I : Integer := Create.C1; -- Raises Constraint_Error
            begin
                Failed
                   ("Improper component selection did not raise Constraint_Error, I =" &
                    Integer'Image (I));
            exception
                when Constraint_Error =>
                    Failed ("Constraint_Error caught by the wrong handler");
            end;

            Failed ("Transfer of control did not happen correctly");

        exception
            when Constraint_Error =>
                if not Finalize_Was_Called then
                    Failed ("Finalize wasn't called when the master was left " &
                            "- Constraint_Error");
                end if;
            when E: others =>
                Failed ("Exception " & Exception_Name (E) &
                        " raised - " & Exception_Information (E));
        end Excep;

    Transfer:
        declare
            Finalize_Was_Called_Before_Leaving_Exit : Boolean;
        begin

            begin
                loop
                    exit when Create.C2 = 3.0;
                end loop;
                Finalize_Was_Called_Before_Leaving_Exit := Finalize_Was_Called;
                if Finalize_Was_Called_Before_Leaving_Exit then
                    Comment ("Finalize called before the transfer of control");
                end if;
            end;

            if not Finalize_Was_Called and then
               not Finalize_Was_Called_Before_Leaving_Exit then
                Failed ("Finalize wasn't called when the master was left " &
                        "- transfer of control");
            end if;
        end Transfer;

    Result;
end C761012;

