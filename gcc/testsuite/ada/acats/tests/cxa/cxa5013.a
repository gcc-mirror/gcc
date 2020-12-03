-- CXA5013.A
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
--      Check that a discrete random number generator will yield each value
--      in its result subtype in a finite number of calls, provided that
--      the number of such values does not exceed 2**15.
--      
-- TEST DESCRIPTION:
--      This test demonstrates certain capabilities of the random number 
--      generator packages in Ada.Numerics.  A generic subprogram is
--      defined that will be instantiated to produce a total of two test
--      subprograms.
--      The area examined by this test is the production of random values 
--      over a discrete range.  A generic procedure is instantiated with
--      an instance of the Discrete_Random package, once for an integer type,
--      and once for an enumeration type.  The test procedure performs a 
--      test run, generating a specific number of random numbers over the 
--      range of the type.  If this run did not generate each of the values
--      in the type range, an asynchronous select statement is invoked.  This
--      select statement has a trigger statement delay for a specific 
--      (implementation defined) amount of time during which additional test
--      runs will be performed.  
--      At the end of each run in this test, an evaluation is made to 
--      determine if each value in the range of possible values have been 
--      generated.  At the conclusion of the runs, or if the specified test
--      delay time expires, the test is concluded with  a status value
--      returned from the test procedure.  An implementation is given three
--      completely separate opportunities to run the test successfully, and 
--      if at the conclusion of all of these tests no successful result has 
--      been returned, the test is considered failed.
--      
--       
-- CHANGE HISTORY:
--      27 Apr 95   SAIC    Initial prerelease version.
--
--!

with Ada.Numerics.Discrete_Random;
with ImpDef;
with Report;

procedure CXA5013 is

begin

   Report.Test ("CXA5013", "Check that a discrete random number generator " &
                           "will yield each value in its result subtype "   &
                           "in a finite number of calls");

   Test_Block:
   declare

      use Ada.Numerics;

      -- The following constant designed into the test creates a high 
      -- probability that a random series of numbers will satisfy the 
      -- requirements. Occasionally, even a random series of numbers 
      -- will fail.  In such a case, the test will reset the random 
      -- number generator and rerun the test conditions.  This constant
      -- determines how many times the random number generator will be
      -- reset before any individual test run is failed.

      TC_Max_Random_Test_Runs       : constant :=    3;

      -- The following constant will ensure that multiple attempts of the
      -- complete set of tests are performed in the event of a failure of 
      -- a set of test runs.

      TC_Finite_Number_Of_Tests : constant :=    3;


      TC_Test_Run               : Integer  :=    0;
      TC_Success                : Boolean  := False;
      TC_Trials_Per_Test        : Integer  := 1500;

      type Enum_Type         is (One, Two, Three, Four, Five, Six, Seven);
      type Discrete_Type     is range 1..100;


      package Enum_Pack      is new Discrete_Random(Enum_Type);
      package Discrete_Pack  is 
        new Discrete_Random(Result_Subtype => Discrete_Type);



      --
      -- Definition of generic Random_Test procedure, which will be 
      -- instantiated for both an integer type and an enumeration type.
      --

      generic
         with package Gen_Pack is new Ada.Numerics.Discrete_Random (<>);
      procedure Random_Test (Trials_Per_Test : in     Integer;
                             Success         :    out Boolean); 


      procedure Random_Test (Trials_Per_Test : in     Integer;
                             Success         :    out Boolean) is
         Total_Runs               : Integer  := 0;
         Total_Trials             : Integer  := 0;
         Total_Attempts_This_Test : Integer  := 0;
         Random_Array             : array (Gen_Pack.Result_Subtype) 
                                      of Boolean := (others => False);
         Gen                      : Gen_Pack.Generator;

         function All_Values_Present return Boolean is
            Result : Boolean := True;
         begin
            for i in Gen_Pack.Result_Subtype'Range loop
               if not Random_Array(i) then 
                  Result := False; 
                  exit;
               end if;
            end loop;
            return Result;
         end All_Values_Present;

      begin

         Success := False;     -- Initialized to failure prior to test.
         Gen_Pack.Reset(Gen);  -- Perform a time-dependent reset.

         -- Guarantee that a specific minimum number of trials are performed
         -- prior to the timer being set.

         for i in 1..Trials_Per_Test loop
            -- Set array element to True when a particular array
            -- index is generated by the random number generator.
            Random_Array(Gen_Pack.Random(Gen)) := True;
         end loop;

         if All_Values_Present then

            Success := True;  -- Test was successful, exit procedure with no
                              -- further testing performed.
         else

            -- Initial test above was unsuccessful, so set a timer and perform
            -- additional trials to determine if all values in the discrete
            -- range will be produced.

            select

               -- This asynchronous select has a triggering statement which
               -- is a delay statement, set to an implementation defined 
               -- number of seconds for any particular test to execute.  
               -- The point here is to allow the implementation to decide 
               -- how long to run this test in order to generate an 
               -- appropriate (i.e., correct) sample from the Random Number
               -- Generator.

               delay ImpDef.Delay_Per_Random_Test;  -- Delay per test.

               -- If, after expiration of delay, the random number generator
               -- has generated all values within the range at least once,
               -- then the result is success; otherwise, a comment is output
               -- to indicate that the random number generator was 
               -- unsuccessful in this series of test runs.

               if All_Values_Present then
                  Success := True;
               else
                  Total_Attempts_This_Test := 
                    Total_Runs * Trials_Per_Test + Total_Trials;
                  Report.Comment
                    ("Not all numbers within the Range were produced in " &
                     Integer'Image(
                       Integer(ImpDef.Delay_Per_Random_Test*1000.0))      &
                     " milliseconds or in "                               &
                     Integer'Image(Total_Attempts_This_Test)              &
                     " trials during this test");
               end if;

            then abort

               -- After setting the triggering statement above, the execution
               -- of this abortable part is begun.
               -- This loop continues until either a) every value has been
               -- produced or b) the triggering statement times out.

               Total_Runs := 1;

               Test_Loop:  -- This loop continues until a test run is
               loop        -- successful, the test run limit has been reached, 
                           -- or the triggering statement times-out above.

                  Total_Trials := 0;

                  for i in 1..Trials_Per_Test loop
                     Total_Trials := i; -- Used above if triggering statement
                                        -- completes prior to test completion.

                     -- Set array element to True when a particular array
                     -- index is generated by the random number generator.

                     Random_Array(Gen_Pack.Random(Gen)) := True;

                  end loop;

                  -- At the conclusion of a complete series of trials, the
                  -- following evaluation is performed to determine whether
                  -- the test run was successful, or whether an additional
                  -- test run should be re-attempted.

                  if All_Values_Present then
                     Success := True;
                     exit Test_Loop;
                  elsif Total_Runs = TC_Max_Random_Test_Runs then
                     Report.Comment
                       ("Not all numbers in the Range were produced in " &
                        Integer'Image(Total_Runs*Trials_Per_Test) &
                        " individual trials during this test");
                     exit Test_Loop;
                  else
                     Total_Runs := Total_Runs + 1;
                  end if;

               end loop Test_Loop;
            end select;
         end if;
      end Random_Test;



      -- Instantiation of test procedures.

      procedure Discrete_Random_Test    is new Random_Test(Discrete_Pack);
      procedure Enumeration_Random_Test is new Random_Test(Enum_Pack);


   begin

      -- Make a series of test runs, checking to ensure that discrete 
      -- random number generators produce each value in their result subtype
      -- within a finite number of calls.  In each case, if the first test
      -- is not successful, another attempt is made, after a time-dependent
      -- reset, up to a total of 3 runs.  This allows an implementation 
      -- multiple opportunities to pass the test successfully.
      -- Note: The odds of getting all 100 integer values in 1500 trials are
      --       greater than 99.997 percent, confirmed by Monte Carlo 
      --       simulation.



      -- Run the Random_Test for an integer discrete random number generator.

      TC_Test_Run := 0;
      TC_Success  := False;
      while TC_Test_Run < TC_Finite_Number_Of_Tests and 
            not TC_Success 
      loop
         TC_Test_Run := TC_Test_Run + 1;            -- Increment test counter.
         Discrete_Random_Test (TC_Trials_Per_Test,  -- Perform test.
                               TC_Success); 
         -- Increment the number of trials that will be performed
         -- in the next test by 50%.
         TC_Trials_Per_Test := TC_Trials_Per_Test + TC_Trials_Per_Test/2 ;
      end loop;

      if not TC_Success then
         Report.Failed("Random_Test was run " & Integer'Image(TC_Test_Run) &
                       " times, but a successful result was not recorded " &
                       "from any run using the integer discrete random "   &
                       "number generator");
      end if;



      -- Run the Random_Test for an enumeration type random number generator.

      -- Note: The odds of getting all seven enumeration values in 100 
      --       trials are greater than 99.997 percent, confirmed by Monte
      --       Carlo simulation.

      TC_Test_Run        := 0;
      TC_Trials_Per_Test := 100;
      TC_Success         := False;
      while TC_Test_Run < TC_Finite_Number_Of_Tests and 
            not TC_Success 
      loop
         TC_Test_Run := TC_Test_Run + 1;
         Enumeration_Random_Test (TC_Trials_Per_Test,
                                  TC_Success); 
         -- Increment the number of trials that will be performed
         -- in the next test by 50%.
         TC_Trials_Per_Test := TC_Trials_Per_Test + TC_Trials_Per_Test/2 ;
      end loop;

      if not TC_Success then
         Report.Failed("Random_Test was run " & Integer'Image(TC_Test_Run) &
                       " times, but a successful result was not recorded " &
                       "from any run using the enumeration random number " &
                       "generator");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA5013;
