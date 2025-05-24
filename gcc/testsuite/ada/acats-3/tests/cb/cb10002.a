-- CB10002.A

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
--      Check that Storage_Error is raised when storage for allocated objects
--      is exceeded.
--
-- TEST DESCRIPTION:
--      This test allocates a very large data structure.
--
--      In order to avoid running forever on virtual memory targets, the
--      data structure is bounded in size, and elements are larger the longer
--      the program runs.
--
--      The program attempts to allocate about 8,600,000 integers, or about
--      32 Megabytes on a typical 32-bit machine.
--
--      If Storage_Error is raised, the data structure is deallocated.
--      (Otherwise, Report.Result may fail as memory is exhausted).

-- CHANGE HISTORY:
--      30 Aug 85   JRK     Ada 83 test created.
--      14 Sep 99   RLB     Created Ada 95 test.


with Report;
with Ada.Unchecked_Deallocation;
procedure CB10002 is

   type Data_Space is array (Positive range <>) of Integer;

   type Element (Size : Positive);

   type Link is access Element;

   type Element (Size : Positive) is
      record
         Parent : Link;
         Child  : Link;
         Sibling: Link;
         Data   : Data_Space (1 .. Size);
      end record;

    procedure Free is new Ada.Unchecked_Deallocation (Element, Link);

    Holder : array (1 .. 430) of Link;
    Last_Allocated : Natural := 0;

    procedure Allocator (Count : in Positive) is
    begin
	-- Allocate various sized objects similar to what a real application
	-- would do.
	if Count in 1 .. 20 then
	    Holder(Count) := new Element (Report.Ident_Int(10));
	elsif Count in 21 .. 40 then
	    Holder(Count) := new Element (Report.Ident_Int(79));
	elsif Count in 41 .. 60 then
	    Holder(Count) := new Element (Report.Ident_Int(250));
	elsif Count in 61 .. 80 then
	    Holder(Count) := new Element (Report.Ident_Int(520));
	elsif Count in 81 .. 100 then
	    Holder(Count) := new Element (Report.Ident_Int(1000));
	elsif Count in 101 .. 120 then
	    Holder(Count) := new Element (Report.Ident_Int(2048));
	elsif Count in 121 .. 140 then
	    Holder(Count) := new Element (Report.Ident_Int(4200));
	elsif Count in 141 .. 160 then
	    Holder(Count) := new Element (Report.Ident_Int(7999));
	elsif Count in 161 .. 180 then
	    Holder(Count) := new Element (Report.Ident_Int(15000));
	else -- 181..430
	    Holder(Count) := new Element (Report.Ident_Int(32000));
	end if;
	Last_Allocated := Count;
    end Allocator;


begin
   Report.Test ("CB10002", "Check that Storage_Error is raised when " &
                           "storage for allocated objects is exceeded");

   begin
      for I in Holder'range loop
         Allocator (I);
      end loop;
      Report.Not_Applicable ("Unable to exhaust memory");
      for I in 1 .. Last_Allocated loop
         Free (Holder(I));
      end loop;
   exception
      when Storage_Error =>
         if Last_Allocated = 0 then
            Report.Failed ("Unable to allocate anything");
         else -- Clean up, so we have enough memory to report on the result.
            for I in 1 .. Last_Allocated loop
               Free (Holder(I));
            end loop;
            Report.Comment (Natural'Image(Last_Allocated) & " items allocated");
         end if;
      when others =>
         Report.Failed ("Wrong exception raised by heap overflow");
   end;

   Report.Result;

end CB10002;
