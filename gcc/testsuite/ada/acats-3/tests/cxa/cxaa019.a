-- CXAA019.A
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
--    Check that Standard_Output can be flushed.  Check that 'in' parameters of
--    types Ada.Text_IO.File_Type and Ada.Streams.Stream_IO.File_Type can be
--    flushed.  (Defect Report 8652/0051).
--
-- CHANGE HISTORY:
--    12 FEB 2001   PHL   Initial version
--    16 MAR 2001   RLB   Readied for release; fixed Not_Applicable check
--                        to terminate test gracefully.
--
--!
with Ada.Streams.Stream_Io;
use Ada.Streams;
with Ada.Text_Io;
with Ada.Wide_Text_Io;
with Report;
use Report;
procedure CXAA019 is

    procedure Check (File : in Ada.Text_Io.File_Type) is
    begin
        Ada.Text_Io.Put_Line
           (File, "   - CXAA019 About to flush a Text_IO file passed " &
                  "as 'in' parameter");
        Ada.Text_Io.Flush (File);
    end Check;

    procedure Check (File : in Ada.Wide_Text_Io.File_Type) is
    begin
        Ada.Wide_Text_Io.Put_Line
           (File, "   - CXAA019 About to flush a Wide_Text_IO file passed " &
                  "as 'in' parameter");
        Ada.Wide_Text_Io.Flush (File);
    end Check;

    procedure Check (File : in Stream_Io.File_Type) is
        S : Stream_Element_Array (1 .. 10);
    begin
        for I in S'Range loop
            S (I) := Stream_Element (Character'Pos ('A') + I);
        end loop;
        Stream_Io.Write (File, S);
        Comment ("About to flush a Stream_IO file passed as 'in' parameter");
        Stream_Io.Flush (File);
    end Check;


begin
    Test ("CXAA019",
          "Check that Standard_Output can be flushed; check that " &
             "'in' Ada.Text_IO.File_Type and Ada.Streams.Stream_IO.File_Type" &
             "parameters can be flushed");

    Ada.Text_Io.Put_Line (Ada.Text_Io.Standard_Output,
                          "   - CXAA019 About to flush Standard_Output");
    Ada.Text_Io.Flush (Ada.Text_Io.Standard_Output);

    Check (Ada.Text_Io.Current_Output);

    declare
        TC_OK : Boolean := False;
        F : Ada.Text_Io.File_Type;
    begin
        begin
            Ada.Text_Io.Create (F, Name => Legal_File_Name (X => 1));
            TC_OK := True;
        exception
            when others =>
                Not_Applicable ("Unable to create Out mode Text_IO file");
        end;
        if TC_OK then
            Check (F);
            Ada.Text_Io.Delete (F);
        end if;
    end;

    declare
        TC_OK : Boolean := False;
        F : Ada.Wide_Text_Io.File_Type;
    begin
        begin
            Ada.Wide_Text_Io.Create (F, Name => Legal_File_Name (X => 2));
            TC_OK := True;
        exception
            when others =>
                Not_Applicable ("Unable to create Out mode Wide_Text_IO file");
        end;
        if TC_OK then
            Check (F);
            Ada.Wide_Text_Io.Delete (F);
        end if;
    end;

    declare
        TC_OK : Boolean := False;
        F : Stream_Io.File_Type;
    begin
        begin
            Stream_Io.Create (F, Name => Legal_File_Name (X => 3));
            TC_OK := True;
        exception
            when others =>
                Not_Applicable ("Unable to create Out mode Stream_IO file");
        end;
        if TC_OK then
            Check (F);
            Stream_Io.Delete (F);
        end if;
    end;

    Result;
end CXAA019;

