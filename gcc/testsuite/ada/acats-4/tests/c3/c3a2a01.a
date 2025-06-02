-- C3A2A01.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
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
--      Check that, for X'Access of a general access type A, Program_Error is
--      raised if the accessibility level of X is deeper than that of A.
--      Check for cases where X'Access occurs in an instance body, and A
--      is passed as an actual during instantiation.
--
-- TEST DESCRIPTION:
--      In order to satisfy accessibility requirements, the designated
--      object X must be at the same or a less deep nesting level than the
--      general access type A -- X must "live" as long as A. Nesting
--      levels are the run-time nestings of masters: block statements;
--      subprogram, task, and entry bodies; and accept statements. Packages
--      are invisible to accessibility rules.
--
--      This test declares three generic units, each of which has a formal
--      general access type:
--
--         (1) A generic package, in which X is declared in the specification,
--             and X'Access occurs within the declarative part of the body.
--
--         (2) A generic package, in which X is a formal in out object of a
--             tagged formal derived type, and X'Access occurs in the sequence
--             of statements of a nested subprogram.
--
--         (3) A generic procedure, in which X is a dereference of an access
--             parameter, and X'Access occurs in the sequence of statements.
--
--      The test verifies the following:
--
--         For (1), Program_Error is raised upon instantiation if the generic
--         package is instantiated at a deeper level than that of the general
--         access type passed as an actual. The exception is propagated to the
--         innermost enclosing master.
--
--         For (2), Program_Error is raised when the nested subprogram is
--         called if the object passed as an actual during instantiation of
--         the generic package has an accessibility level deeper than that of
--         the general access type passed as an actual. The exception is
--         handled within the nested subprogram. Also, check that
--         Program_Error is not raised if the level of the actual access type
--         is deeper than that of the actual object.
--
--         For (3), Program_Error is raised when the instance subprogram is
--         called if the object pointed to by the actual corresponding to
--         the access parameter has an accessibility level deeper than that of
--         the general access type passed as an actual during instantiation.
--         The exception is handled within the instance subprogram. Also,
--         check that Program_Error is not raised if the level of the actual
--         access type is deeper than that of the actual corresponding to the
--         access parameter.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         F3A2A00.A
--      -> C3A2A01.A
--
--
-- CHANGE HISTORY:
--      12 May 95   SAIC    Initial prerelease version.
--      10 Jul 95   SAIC    Modified code to avoid dead variable optimization.
--
--!

with F3A2A00;
generic
   type FD  is new F3A2A00.Array_Type;
   type FAF is access all FD;
package C3A2A01_0 is
   X : aliased FD;

   procedure Dummy;  -- Needed to allow package body.
end C3A2A01_0;


     --==================================================================--


with Report;
package body C3A2A01_0 is
   Ptr   : FAF     := X'Access;
   Index : Integer := F3A2A00.Array_Type'First;

   procedure Dummy is
   begin
      null;
   end Dummy;
begin
   -- Avoid optimization (dead variable removal of Ptr):

   if not Report.Equal (Ptr(Index).C, Ptr(Index).C) then   -- Always false.
      Report.Failed ("Unexpected error in C3A2A01_0 instance");
   end if;
end C3A2A01_0;


     --==================================================================--


with F3A2A00;
generic
   type FD  is new F3A2A00.Tagged_Type with private;
   type FAF is access all FD;
   FObj : in out FD;
package C3A2A01_1 is
   procedure Handle (R: out F3A2A00.TC_Result_Kind);
end C3A2A01_1;


     --==================================================================--


with Report;
package body C3A2A01_1 is

   procedure Handle (R: out F3A2A00.TC_Result_Kind) is
      Ptr : FAF;
   begin
      Ptr := FObj'Access;
      R   := F3A2A00.OK;

      -- Avoid optimization (dead variable removal of Ptr):

      if not Report.Equal (Ptr.C, Ptr.C) then              -- Always false.
         Report.Failed ("Unexpected error in Handle");
      end if;
   exception
      when Program_Error => R := F3A2A00.P_E;
      when others        => R := F3A2A00.O_E;
   end Handle;

end C3A2A01_1;


     --==================================================================--


with F3A2A00;
generic
   type FD  is new F3A2A00.Array_Type;
   type FAF is access all FD;
procedure C3A2A01_2 (P: access FD; R: out F3A2A00.TC_Result_Kind);


     --==================================================================--


with Report;
procedure C3A2A01_2 (P: access FD; R: out F3A2A00.TC_Result_Kind) is
   Ptr   : FAF;
   Index : Integer := F3A2A00.Array_Type'First;
begin
   Ptr := P.all'Access;
   R   := F3A2A00.OK;

   -- Avoid optimization (dead variable removal of Ptr):

   if not Report.Equal (Ptr(Index).C, Ptr(Index).C) then   -- Always false.
      Report.Failed ("Unexpected error in C3A2A01_2 instance");
   end if;
exception
   when Program_Error => R := F3A2A00.P_E;
   when others        => R := F3A2A00.O_E;
end C3A2A01_2;


     --==================================================================--


with F3A2A00;
with C3A2A01_0;
with C3A2A01_1;
with C3A2A01_2;

with Report;
procedure C3A2A01 is
begin -- C3A2A01.                                              -- [ Level = 1 ]

   Report.Test ("C3A2A01", "Run-time accessibility checks: instance " &
                "bodies. Type of X'Access is passed as actual to instance");


   SUBTEST1:
   declare                                                     -- [ Level = 2 ]
      Result : F3A2A00.TC_Result_Kind;
   begin -- SUBTEST1.

      declare                                                  -- [ Level = 3 ]
         type AccArr_L3 is access all F3A2A00.Array_Type;
      begin
         declare                                               -- [ Level = 4 ]
            -- The accessibility level of Pack.X is that of the instantiation
            -- (4). The accessibility level of the actual access type used to
            -- instantiate Pack is 3. Therefore, the X'Access in Pack
            -- propagates Program_Error when the instance body is elaborated:

            package Pack is new C3A2A01_0 (F3A2A00.Array_Type, AccArr_L3);
         begin
            Result := F3A2A00.OK;
         end;
      exception
         when Program_Error => Result := F3A2A00.P_E;       -- Expected result.
         when others        => Result := F3A2A00.O_E;
      end;

      F3A2A00.TC_Display_Results (Result, F3A2A00.P_E, "SUBTEST #1");

   end SUBTEST1;



   SUBTEST2:
   declare                                                     -- [ Level = 2 ]
      Result : F3A2A00.TC_Result_Kind;
   begin -- SUBTEST2.

      declare                                                  -- [ Level = 3 ]
         -- The instantiation of C3A2A01_1 should NOT result in any
         -- exceptions.

         type AccTag_L3 is access all F3A2A00.Tagged_Type;

         package Pack_OK is new C3A2A01_1 (F3A2A00.Tagged_Type,
                                           AccTag_L3,
                                           F3A2A00.X_L0);
      begin
         -- The accessibility level of the actual object used to instantiate
         -- Pack_OK is 0. The accessibility level of the actual access type
         -- used to instantiate Pack_OK is 3. Therefore, the FObj'Access in
         -- Pack_OK.Handle does not raise an exception when the subprogram is
         -- called. If an exception is (incorrectly) raised, however, it is
         -- handled within the subprogram:

         Pack_OK.Handle (Result);
      end;

      F3A2A00.TC_Display_Results (Result, F3A2A00.OK, "SUBTEST #2");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #2: Program_Error incorrectly raised " &
                        "during instantiation of generic");
      when others        =>
         Report.Failed ("SUBTEST #2: Unexpected exception raised " &
                        "during instantiation of generic");
   end SUBTEST2;



   SUBTEST3:
   declare                                                     -- [ Level = 2 ]
      Result : F3A2A00.TC_Result_Kind;
   begin -- SUBTEST3.

      declare                                                  -- [ Level = 3 ]
         -- The instantiation of C3A2A01_1 should NOT result in any
         -- exceptions.

         X_L3: F3A2A00.Tagged_Type;

         package Pack_PE is new C3A2A01_1 (F3A2A00.Tagged_Type,
                                           F3A2A00.AccTag_L0,
                                           X_L3);
      begin
         -- The accessibility level of the actual object used to instantiate
         -- Pack_PE is 3. The accessibility level of the actual access type
         -- used to instantiate Pack_PE is 0. Therefore, the FObj'Access in
         -- Pack_OK.Handle raises Program_Error when the subprogram is
         -- called. The exception is handled within the subprogram:

         Pack_PE.Handle (Result);
      end;

      F3A2A00.TC_Display_Results (Result, F3A2A00.P_E, "SUBTEST #3");

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #3: Program_Error incorrectly raised " &
                        "during instantiation of generic");
      when others        =>
         Report.Failed ("SUBTEST #3: Unexpected exception raised " &
                        "during instantiation of generic");
   end SUBTEST3;



   SUBTEST4:
   declare                                                     -- [ Level = 2 ]
      Result1 : F3A2A00.TC_Result_Kind;
      Result2 : F3A2A00.TC_Result_Kind;
   begin -- SUBTEST4.

      declare                                                  -- [ Level = 3 ]
         -- The instantiation of C3A2A01_2 should NOT result in any
         -- exceptions.

         X_L3: aliased F3A2A00.Array_Type;
         type AccArr_L3 is access all F3A2A00.Array_Type;

         procedure Proc is new C3A2A01_2 (F3A2A00.Array_Type, AccArr_L3);
      begin
         -- The accessibility level of Proc.P.all is that of the corresponding
         -- actual during the call (in this case 3). The accessibility level of
         -- the access type used to instantiate Proc is also 3. Therefore, the
         -- P.all'Access in Proc does not raise an exception when the
         -- subprogram is called. If an exception is (incorrectly) raised,
         -- however, it is handled within the subprogram:

         Proc (X_L3'Access, Result1);

         F3A2A00.TC_Display_Results (Result1, F3A2A00.OK,
                                     "SUBTEST #4: same levels");

         declare                                               -- [ Level = 4 ]
            X_L4: aliased F3A2A00.Array_Type;
         begin
            -- Within this block, the accessibility level of the actual
            -- corresponding to Proc.P.all is 4. Therefore, the P.all'Access
            -- in Proc raises Program_Error when the subprogram is called. The
            -- exception is handled within the subprogram:

            Proc (X_L4'Access, Result2);

            F3A2A00.TC_Display_Results (Result2, F3A2A00.P_E,
                                        "SUBTEST #4: object at deeper level");
         end;

      end;

   exception
      when Program_Error =>
         Report.Failed ("SUBTEST #4: Program_Error incorrectly raised " &
                        "during instantiation of generic");
      when others        =>
         Report.Failed ("SUBTEST #4: Unexpected exception raised " &
                        "during instantiation of generic");
   end SUBTEST4;


   Report.Result;

end C3A2A01;
