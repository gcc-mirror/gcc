-- C650002.A
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
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--     Check that the completion of a return statement that applies to an
--     accept statement causes the accept statement to return.
--
--     Check that the completion of a return statement that applies to an
--     entry body causes the entry to return.
--
-- TEST DESCRIPTION:
--     A protected object is used to control access to a shared data structure
--     that records the result of processing various "jobs". The data structure
--     has a Start_Job entry; this entry is called by each task before starting
--     a job. If the job has already been started (presumably by another task),
--     a parameter is set with that information and the entry body
--     immediately returns using a simple_return_statement. The protected
--     object also includes a Record_Job_Result procedure that records the
--     results (for this test, the result is a simple float value).
--
--     A worker task type for processing jobs is created. It has an entry for
--     accepting jobs to process. The body of the task is just a loop to
--     accept and then process jobs. If that job
--     has already been processed, the accept statement immediately returns.
--
--     The main program creates several worker tasks, then gives them
--     a variety of jobs (simulating a job scheduler). These exercise all
--     of the paths in the worker tasks and the protected object.
--
-- CHANGE HISTORY:
--     26 Mar 2008 RLB Created test to fill in missing Ada 95 tests.
--
with Report;
procedure C650002 is

   type Data_Item is record
      Job : Natural;
      Result : Float;
   end record;
   Data : array (1..10) of Data_Item;
      -- The data structure protected by

   protected Sentry is
      entry Start_Job (Job : in Natural; Previously_Started : out Boolean);
      procedure Record_Job_Result (Job : in Natural; Result : in Float);
   private
      Num_Started_Jobs : Natural := Data'First-1;
   end Sentry;

   protected body Sentry is
      entry Start_Job (Job : in Natural; Previously_Started : out Boolean)
         when True is
      begin
         for I in Data'First .. Num_Started_Jobs loop
            if Job = Data(I).Job then
                -- Job already started.
                Previously_Started := True;
                return;
            end if;
         end loop;
         Previously_Started := False;
         Num_Started_Jobs := Num_Started_Jobs + 1;
         Data(Num_Started_Jobs).Job := Job;
      end Start_Job;

      procedure Record_Job_Result (Job : in Natural; Result : in Float) is
      begin
         for I in Data'First .. Num_Started_Jobs loop
            if Job = Data(I).Job then
                -- Found job.
                Data(I).Result := Result;
                return;
            end if;
         end loop;
         raise Program_Error; -- Could not find job.
      end Record_Job_Result;

   end Sentry;

   task type Worker is
       entry Start_Job (Job : in Natural; TC_Previously_Started : in Boolean);
   end Worker;

   task body Worker is
      Our_Job : Natural;
      Previously_Started : Boolean;
   begin
      loop
         select
            accept Start_Job (Job : in Natural; TC_Previously_Started : in Boolean) do
               Sentry.Start_Job (Job, Previously_Started);
               if Previously_Started then
                  if not TC_Previously_Started then
                     Report.Failed("Job # " & Natural'Image(Job) & " incorrectly reported as previously started");
                  end if;
                  return; -- Nothing to do.
               end if;
               -- Job setup here.
               Our_Job := Job;
               if TC_Previously_Started then
                  if Previously_Started then
                     Report.Failed("Job # " & Natural'Image(Job) & " - return statement ignored");
                  else
                     Report.Failed("Job # " & Natural'Image(Job) & " previously started, but processing again anyway");
                  end if;
               end if;
            end Start_Job;
            delay 0.5; -- Simulate expensive job processing.
            begin
               Sentry.Record_Job_Result (Job => Our_Job, Result => Float(Our_Job) * 3.5);
            exception
               when Program_Error =>
                  Report.Failed("Could not find job to report result: #" & Natural'Image(Our_Job));
            end;
         or
            terminate;
         end select;
      end loop;
   end Worker;

   Work1, Work2 : Worker;

begin
   Report.Test ("C650002", "Check that a return statement in an entry body " &
                           "or accept statement completes the body or statement");

   -- Schedule jobs:
   Work1.Start_Job (Job =>  4, TC_Previously_Started => False);
   Work2.Start_Job (Job => 92, TC_Previously_Started => False);
   Work1.Start_Job (Job => 66, TC_Previously_Started => False);
   Work2.Start_Job (Job =>  4, TC_Previously_Started => True); -- A repeat job.
   Work1.Start_Job (Job => 15, TC_Previously_Started => False);
   Work2.Start_Job (Job => 66, TC_Previously_Started => True); -- Another repeat job.

   -- Check the data results:
   if Data(1).Job /= 4 then
      Report.Failed ("First job processed wrong: was #" & Natural'Image(Data(1).Job) &
                     " should be # 4");
   end if;
   if Data(2).Job /= 92 then
      Report.Failed ("Second job processed wrong: was #" & Natural'Image(Data(2).Job) &
                     " should be # 92");
   end if;
   if Data(3).Job /= 66 then
      Report.Failed ("Third job processed wrong: was #" & Natural'Image(Data(3).Job) &
                     " should be # 66");
   end if;
   if Data(4).Job /= 15 then
      Report.Failed ("Fourth job processed wrong: was #" & Natural'Image(Data(4).Job) &
                     " should be # 15");
   end if;

   Report.Result;

end C650002;

