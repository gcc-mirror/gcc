-- CXAI032.A
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
--
-- OBJECTIVE:
--      Test cursor iterators using function Iterate for ordered maps.
--
-- TEST DESCRIPTION:
--      This test maps cities names with a distance (from Paris)
--      Check that values are returned in order.
--      Check "in" form of loop, with and without a Start parameter, and exiting
--      the loop early. Check tampering.
--
-- CHANGE HISTORY:
--      11 OCT 13   JPR     ACATS 4.0
--      16 OCT 13   RLB     Fixed headers and punctuation.
--       4 APR 14   RLB     Renamed test to create ACATS 4.0 version, improved
--                          objective.
--
--!

with Ada.Containers.Ordered_Maps;

with Report;
with Ada.Exceptions;
procedure CXAI032 is
   type Distance is digits 5 range 0.0 .. 40.0E6;
   Km : constant Distance := 1000.0;
   subtype City is String (1..10);

   package City_Distances is new Ada.Containers.Ordered_Maps (City, Distance);
   use City_Distances;

   Distances : City_Distances.Map;
   Saved     : City_Distances.Map;
   Temp      : City_Distances.Map;
   Previous  : City;
   Iteration : Natural;
   Marks     : array (Natural range 1..6) of Boolean := (others => False); -- 6 = number of data

   procedure Tamperer (Key : City; Element : Distance) is
   begin
      Distances.Delete (Key);
   end Tamperer;

begin
   Report.Test ("CXAI032",
                "Test cursor iterators using function Iterate for ordered " &
                "maps");

   -- Initialization: some distances from Paris
   -- Entered in order of increasing distances to make sure that keys are not initially sorted
   Insert (Distances, "NOGENT    ",  167.00*Km);
   Insert (Distances, "LONDON    ",  343.15*Km);
   Insert (Distances, "STUTTGART ",  505.54*Km);
   Insert (Distances, "BOSTON    ", 5537.51*Km);
   Insert (Distances, "NEW-YORK  ", 5843.60*Km);
   Insert (Distances, "MADISON   ", 6682.15*Km);

   -- Check that cities are returned in order, from First to Last
   Previous  := (others => ' ');
   Iteration := 0;
   for C in Distances.Iterate loop
      Iteration := Iteration + 1;
      if Iteration = 1 and then
	(Key (C) /= First_Key (Distances)
	 or Distances (C) /= First_Element (Distances))
      then
	 Report.Failed ("Incorrect first element (forward case)");
      end if;

      if Previous >= Key (C) then
	 Report.Failed ("Keys not returned in order (forward case)");
      end if;

      if Iteration = 6 and then
	(Key (C) /= Last_Key (Distances)
	   or Distances (C) /= Last_Element (Distances))
      then
	 Report.Failed ("Incorrect last element (forward case)");
      end if;

      Previous := Key (C);
   end loop;
   if Iteration /= 6 then
      Report.Failed ("Incorrect number of iterations (forward case)");
   end if;

   -- Id., in reverse order
   Previous := (others => Character'Last);
   Iteration := 0;
   for C in reverse Distances.Iterate loop
      Iteration := Iteration + 1;
      if Iteration = 1 and then
	(Key (C) /= Last_Key (Distances)
	 or Distances (C) /= Last_Element (Distances))
      then
	 Report.Failed ("Incorrect first element (reverse case)");
      end if;

      if Previous <= Key (C) then
	 Report.Failed ("Keys not returned in order (reverse case)");
      end if;

      if Iteration = 6 and then
	(Key (C) /= First_Key (Distances)
	   or Distances (C) /= First_Element (Distances))
      then
	 Report.Failed ("Incorrect last element (reverse case)");
      end if;

      Previous := Key (C);
   end loop;
   if Iteration /= 6 then
      Report.Failed ("Incorrect number of iterations (reverse case)");
   end if;

   -- Incomplete loop: from London to Nogent
   Iteration := 0;
   for C in Distances.Iterate (Find (Distances, "LONDON    ")) loop
      Iteration := Iteration + 1;
      if Iteration = 1 and then Key (C) /= "LONDON    " then
	 Report.Failed ("Incorrect first element (different start)");
      end if;

      if Iteration = 4 then
	if Key (C) /= "NOGENT    " then
	 REPORT.Failed ("Incorrect last element (different start)");
	end if;
	exit;
      end if;
   end loop;
   if Iteration /= 4 then
      Report.Failed ("Incorrect number of iterations (different start)");
   end if;

   -- Incomplete loop: reverse from London to Boston
   Iteration := 0;
   for C in reverse Distances.Iterate (Find (Distances, "LONDON    ")) loop
      Iteration := Iteration + 1;
      if Iteration = 1 and then Key (C) /= "LONDON    " then
	 Report.Failed ("Incorrect first element (different start)");
      end if;

      if Iteration = 2 then
	if Key (C) /= "BOSTON    " then
	 REPORT.Failed ("Incorrect last element (different start)");
	end if;
	exit;
      end if;
   end loop;
   if Iteration /= 2 then
      Report.Failed ("Incorrect number of iterations (different start)");
   end if;

   -- Tampering
   Iteration := 0;
   Assign (Saved, Distances);
   for C in Distances.Iterate loop
      Iteration := Iteration + 1;
      begin
	 -- Check a different tampering on each iteration
	 case Iteration is
	    when 1 =>
	       Distances.Delete ("MADISON   ");    -- Tampering with cursor
	       Report.Failed ("Tampering did not raise exception, case 1");
	    when 2 =>
	       Move (Temp, Distances);    -- Tampering with cursor
	       Report.Failed ("Tampering did not raise exception, case 2");
	    when 3 =>
	       Insert (Distances, "PADOVA    ", 816.46*Km);    -- Tampering with cursor
	       Report.Failed ("Tampering did not raise exception, case 3");
	    when 4 =>
	       Clear (Distances);    -- Tampering with cursor
	       Report.Failed ("Tampering did not raise exception, case 4");
	    when 5 =>
	       Query_Element (C, Tamperer'Access);  -- Tampering with elements, prohibited during Query_Element
	       Report.Failed ("Tampering did not raise exception, case 5");
	    when 6 =>
	       Distances.Replace ("NOGENT    ", 133.34*Km);  -- Tampering with elements allowed here
	       Replace_Element (Distances, C, 624.00*Km);    -- C designates STUTTGART
	    when others =>
	       Report.Failed ("Extra iteration in tampering check");
	 end case;

      exception
	 when Program_Error =>
	    if Iteration = 6 then
	       Report.Failed ("Tampering with elements raised Program_Error");
	    end if;
	 when Occur: others =>
	    Report.Failed ("Tampering raised other exception: " & Ada.Exceptions.Exception_Name (Occur));
      end;
      Marks (Iteration) := True;
   end loop;

   Replace (Saved, "NOGENT    ", 133.34*Km);
   Replace (Saved, "STUTTGART ", 624.00*Km);
   if Distances /= Saved then -- Tampering should not change map
      Report.Failed ("Tampering modified map");
   end if;
   if Marks /= (Marks'Range => True) then
      Report.Failed ("Tampering prevented some iterations");
   end if;


   Report.Result;

exception
      when Occur : others =>
	 Report.Failed ("Exception at global level: " & Ada.Exceptions.Exception_Name (Occur));
	 Report.Comment ("Exception message: " & Ada.Exceptions.Exception_Message (Occur));
	 Report.Result;
end CXAI032;
