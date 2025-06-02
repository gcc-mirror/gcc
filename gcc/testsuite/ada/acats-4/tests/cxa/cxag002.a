-- CXAG002.A
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
--
--*
-- OBJECTIVE:
--     Check that package Ada.Directories.Hierarchical_File_Names exists and
--     the functions it contains work as expected.
--
-- TEST DESCRIPTION:
--     We test the "algebra" of file name construction and decomposition.
--
--     The pair Simple_Name and Containing_Directory decompose file names
--     one path segment at a time, from the right end of the name. The pair
--     Initial_Directory and Relative_Name decompose file names
--     one path segment at a time, from the left end of the name. Compose
--     constructs a name out of parts.
--
--     We use the notation Full(n) and Rel(n) to represent full names with
--     n path segments and relative names with n path segments respectively.
--     (The AARM also uses this notation in some notes.) The various operations
--     are described using this notation, and results are checked against
--     it as well as directly checking the results.
--
-- APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support all of the
--     following:
--        * Package Ada.Directories.Hierarchical_File_Names; and
--        * Reading the current default directory.
--
-- CHANGE HISTORY:
--     19 Feb 2015  RLB  Created test.
--
--!
with Report;
with Ada.Directories.Hierarchical_File_Names;  -- N/A => ERROR.
with Impdef;
with Ada.Exceptions;
procedure CXAG002 is

   TC_Trace : constant Boolean := False; -- Output extra trace information.

   package ADH renames Ada.Directories.Hierarchical_File_Names;

   type Str_Ptr is access String;

   Default_Directory : Str_Ptr;

   File1_Simple_Name : Str_Ptr;
   File2_Simple_Name : Str_Ptr;

   Working1_Name : Str_Ptr;
   Working2_Name : Str_Ptr;
   Working3_Name : Str_Ptr;

   function Path_Segments (Name : in String) return Natural is
      -- Return the count of path segments in Name, decomposing
      -- from the right end.
   begin
      if ADH.Is_Simple_Name (Name) or else
         ADH.Is_Root_Directory_Name (Name) or else
         ADH.Is_Parent_Directory_Name (Name) or else
         ADH.Is_Current_Directory_Name (Name) then
          return 1; -- Can't decompose further.
      else
          return Path_Segments (ADH.Containing_Directory (Name)) + 1;
      end if;
   end Path_Segments;


   function Alt_Path_Segments (Name : in String) return Natural is
      -- Return the count of path segments in Name, decomposing
      -- from the right end.
   begin
      if ADH.Is_Simple_Name (Name) or else
         ADH.Is_Root_Directory_Name (Name) or else
         ADH.Is_Parent_Directory_Name (Name) or else
         ADH.Is_Current_Directory_Name (Name) then
          return 1; -- Can't decompose further.
      else
          return Path_Segments (ADH.Relative_Name (Name)) + 1;
      end if;
   end Alt_Path_Segments;


   procedure Check_Relations (Name    : in String;
                              Subtest : in String) is
      -- Check the relationships between operations on Name.

      procedure Single_Check is
         -- Make checks for a single entity.
         Init_Dir : constant String := ADH.Initial_Directory (Name);
      begin
         if not Impdef.Equivalent_File_Names (Name,
             Init_Dir) then
            Report.Failed ("Initial directory not same as Name - " &
                            Subtest);
         end if;
         begin
             if Impdef.Equivalent_File_Names (Name,
                 ADH.Relative_Name (Name)) then
                Report.Failed ("Relative_Name had no effect - " &
                                Subtest);
             else
                Report.Failed ("Relative_Name did not raise Name_Error - " &
                                Subtest);
             end if;
         exception
            when Ada.Directories.Name_Error =>
               if TC_Trace then
                  Report.Comment ("Name_Error as expected from " &
                                  "relative name of single part");
               end if;
            when Ada.Directories.Use_Error =>
               Report.Failed ("Wrong exception from relative name " &
                               "of single part - " &
                               Subtest);
         end;
         begin
             if Impdef.Equivalent_File_Names (Name,
                 ADH.Containing_Directory (Name)) then
                Report.Failed ("Containing_Directory had no effect - " &
                                Subtest);
             elsif ADH.Is_Simple_Name (Name) then
                Report.Comment ("Containing_Directory of simple name " &
                   "returns " & ADH.Containing_Directory (Name) &
                   " - " & Subtest);
                   -- The AARM encourages returning "." in this case, so
                   -- we allow that.
             else
                Report.Failed ("Containing_Directory did not raise " &
                               "Use_Error - " & Subtest);
             end if;
         exception
            when Ada.Directories.Name_Error =>
               Report.Failed ("Wrong exception from containing directory " &
                               "of single part - " &
                               Subtest);
            when Ada.Directories.Use_Error =>
               if TC_Trace then
                  Report.Comment ("Use_Error as expected from " &
                                  "containing directory of single part");
               end if;
         end;
         begin
             if Impdef.Equivalent_File_Names (Name,
                 ADH.Simple_Name (Name)) then
                if TC_Trace then
                    Report.Comment ("  Simple_Name is identity on " &
                                      "single part");
                end if;
             else
                Report.Failed ("Simple_Name gets non-identity result - " &
                                Subtest);
             end if;
         exception
            when Ada.Directories.Name_Error =>
               Report.Comment ("Name_Error from Simple_Name " &
                                "of single part");
               -- We allow this as the result might not be Is_Simple_Name.
            when Ada.Directories.Use_Error =>
               Report.Failed ("Wrong exception from simple name " &
                               "of single part - " &
                               Subtest);
         end;
      end Single_Check;

   begin
      if TC_Trace then
         Report.Comment ("Check relations for " & Name);
      end if;

      -- Check the we can actually decompose the Name.
      if ADH.Is_Parent_Directory_Name (Name) then
         if TC_Trace then
            Report.Comment ("  Is parent directory name");
         end if;
         Single_Check;
         return;
      elsif ADH.Is_Current_Directory_Name (Name) then
         if TC_Trace then
            Report.Comment ("  Is current directory name");
         end if;
         Single_Check;
         return;
      elsif ADH.Is_Simple_Name (Name) then
         if TC_Trace then
            Report.Comment ("  Is simple name");
         end if;
         Single_Check;
         return;
      elsif ADH.Is_Root_Directory_Name (Name) then
         if TC_Trace then
            Report.Comment ("  Is root directory name");
         end if;
         Single_Check;
         return;
      -- else OK, can decompose, go on.
      end if;

      -- Relative_Name/Initial_Directory
      -- If Name = Full/Rel(n), then Relative_Name(Name) = Rel(n-1) and
      -- Initial_Directory = Full/Rel(1).

      declare
         Init_Dir : constant String := ADH.Initial_Directory (Name);
         Rel_Name : constant String := ADH.Relative_Name (Name);
      begin
         if TC_Trace then
            Report.Comment ("  Initial_Directory is " & Init_Dir);
            Report.Comment ("  Relative_Name is " & Rel_Name);
         end if;
         if ADH.Is_Full_Name (Name) then
            if not ADH.Is_Root_Directory_Name (Init_Dir) then
               Report.Failed ("Initial_Directory not root for full name - " &
                               Subtest);
            -- else OK.
            end if;
         else -- Relative Name
            if ADH.Is_Parent_Directory_Name (Init_Dir) then
               null; -- OK.
            elsif ADH.Is_Current_Directory_Name (Init_Dir) then
               null; -- OK.
            elsif ADH.Is_Simple_Name (Init_Dir) then
               null; -- OK.
            else
               Report.Failed ("Initial_Directory not simple for " &
                              "relative name - " & Subtest);
            end if;
         end if;
         if not ADH.Is_Relative_Name (Rel_Name) then
            Report.Failed ("Relative_Name not relative name - " &
                            Subtest);
         -- else OK.
         end if;
         if Path_Segments (Init_Dir) /= 1 then
            Report.Failed ("Wrong number of segments for " &
                           "Initial_Directory - " & Subtest);
         end if;
         if Path_Segments (Rel_Name) /= Path_Segments (Name) - 1 then
            Report.Failed ("Wrong number of segments for Relative_Name - " &
                            Subtest);
         end if;
         if not Impdef.Equivalent_File_Names (Name,
             ADH.Compose (Init_Dir, Rel_Name)) then
            Report.Failed ("Compose failed to put Humpty Dumpty back " &
                           "together again - " & Subtest);
         end if;
      end;

      -- Simple_Name/Containing_Directory
      -- If Name = Full/Rel(n),
      -- then Containing_Directory(Name) = Full/Rel(n-1) and
      --    Simple_Name = Rel(1).

      declare
         Cont_Dir : constant String := ADH.Containing_Directory (Name);
         Simple_Name : constant String := ADH.Simple_Name (Name);
      begin
         if TC_Trace then
            Report.Comment ("  Containing_Directory is " & Cont_Dir);
            Report.Comment ("  Simple_Name is " & Simple_Name);
         end if;
         if ADH.Is_Full_Name (Name) then
            if not ADH.Is_Full_Name (Cont_Dir) then
               Report.Failed ("Containing_Directory not full name for " &
                              "full name - " & Subtest);
            -- else OK.
            end if;
         else -- Relative name
            if not ADH.Is_Relative_Name (Cont_Dir) then
               Report.Failed ("Containing_Directory not relative name for " &
                              "relative name - " & Subtest);
            end if;
         end if;
         if not ADH.Is_Simple_Name (Simple_Name) then
            Report.Failed ("Simple_Name not simple name - " &
                            Subtest);
         -- else OK.
         end if;
         if Alt_Path_Segments (Simple_Name) /= 1 then
            Report.Failed ("Wrong number of segments for Simple_Name - " &
                            Subtest);
         end if;
         if Alt_Path_Segments (Cont_Dir) /= Alt_Path_Segments (Name) - 1 then
            Report.Failed ("Wrong number of segments for " &
                           "Containing_Directory - " & Subtest);
         end if;
         if not Impdef.Equivalent_File_Names (Name,
             ADH.Compose (Cont_Dir, Simple_Name)) then
            Report.Failed ("Compose failed reconstruction - " &
                            Subtest);
         end if;
      end;

      -- Compose
      -- If Name = Full/Rel(n), then
      -- Compose(Name, Simple) = Full/Rel(n+1).

      declare
         Comp : constant String := ADH.Compose (Name, File1_Simple_Name.all);
      begin
         if TC_Trace then
            Report.Comment ("  Compose is " & Comp);
         end if;
         if ADH.Is_Full_Name (Name) then
            if not ADH.Is_Full_Name (Comp) then
               Report.Failed ("Compose not full name for full name - " &
                               Subtest);
            -- else OK.
            end if;
         else -- Relative name
            if not ADH.Is_Relative_Name (Comp) then
               Report.Failed ("Compose not relative name for " &
                              "relative name - " & Subtest);
            end if;
         end if;
         if not Impdef.Equivalent_File_Names (File1_Simple_Name.all,
             ADH.Simple_Name (Comp)) then
            Report.Failed ("Wrong simple name for Compose - " &
                            Subtest);
         end if;
         if not Impdef.Equivalent_File_Names (Name,
             ADH.Containing_Directory (Comp)) then
            Report.Failed ("Wrong containing directory for Compose - " &
                            Subtest);
         end if;
         if Path_Segments (Comp) /= Path_Segments (Name) + 1 then
            Report.Failed ("Wrong number of segments for Compose - " &
                            Subtest);
         end if;
      end;

      -- Compose
      -- If Name = Rel(n), then Compose(Simple, Name) = Rel(n+1).
      if ADH.Is_Relative_Name (Name) then
         declare
            Comp2 : constant String :=
               ADH.Compose (File1_Simple_Name.all, Name);
         begin
            if not ADH.Is_Relative_Name (Comp2) then
               Report.Failed ("Compose(Simple+Relative) not " &
                              "relative name - " & Subtest);
            end if;
            if not Impdef.Equivalent_File_Names (File1_Simple_Name.all,
                ADH.Initial_Directory (Comp2)) then
               Report.Failed ("Wrong initial directory for Compose - " &
                            Subtest);
            end if;
            if not Impdef.Equivalent_File_Names (Name,
                ADH.Relative_Name (Comp2)) then
               Report.Failed ("Wrong relative name for Compose - " &
                               Subtest);
            end if;
            if Alt_Path_Segments (Comp2) /= Alt_Path_Segments (Name) + 1 then
               Report.Failed ("Wrong number of segments for " &
                              "Compose(Simple+Relative) - " &
                               Subtest);
            end if;
         end;
      else
         begin
             if Impdef.Equivalent_File_Names (Name,
                 ADH.Compose (File1_Simple_Name.all, Name)) then
                Report.Failed ("Compose had no effect - " &
                                Subtest);
             else
                Report.Failed ("Compose did not raise Name_Error - " &
                                Subtest);
             end if;
         exception
            when Ada.Directories.Name_Error =>
               if TC_Trace then
                  Report.Comment ("Name_Error as expected from composing " &
                                  "something to a full name");
               end if;
            when Ada.Directories.Use_Error =>
               Report.Failed ("Wrong exception from composing " &
                               "something to a full name - " &
                               Subtest);
         end;
      end if;
   end Check_Relations;


begin
   Report.Test ("CXAG002", "Check that package " &
                           "Ada.Directories.Hierarchical_File_Names exists " &
                           "and the functions it contains work as expected");

   begin
      Default_Directory := new String'(Ada.Directories.Current_Directory);
         -- We use this to find a possible directory root.
      if TC_Trace then
         Report.Comment ("Default directory=" & Default_Directory.all);
      end if;
   exception
      when Ada.Directories.Use_Error =>
         Report.Not_Applicable ("Unable to retrieve " &
                                "current default directory");
         goto Complete;
   end;

   -- Get some simple names from Report:
   File1_Simple_Name := new String'(Report.Legal_File_Name (X => 1));
   File2_Simple_Name := new String'(Report.Legal_File_Name (X => 2));

   if not ADH.Is_Parent_Directory_Name (Impdef.Parent_Directory_Name) then
      Report.Failed ("Specified parent directory=" &
                      Impdef.Parent_Directory_Name & " is not that");
   end if;

   if not ADH.Is_Current_Directory_Name (Impdef.Current_Directory_Name) then
      Report.Failed ("Specified current directory=" &
                      Impdef.Current_Directory_Name & " is not that");
   end if;

   if ADH.Is_Current_Directory_Name (Default_Directory.all) then
      Report.Failed ("Confused current directory name and name of " &
                     "current default directory");
         -- The name of the current default directory is always a full name
         -- (an implementation of Ada.Directories.Current_Directory that
         -- always returned "." on Linux would be wrong), while the
         -- current directory name is always a simple name.
   end if;

   if ADH.Is_Root_Directory_Name (Default_Directory.all) then
      -- It would be weird, but possible to run the ACATS in a root.
      if TC_Trace then
         Report.Comment ("Default directory is a root");
      end if;
   elsif ADH.Is_Parent_Directory_Name
            (ADH.Containing_Directory (Default_Directory.all)) then
      Report.Failed ("Confused parent directory name and name of " &
                     "parent of current default directory");
         -- The name of the containing directoty of the current default
         -- directory is always a full name, while the
         -- parent directory name is always a simple name.
   end if;

   if TC_Trace then
       Report.Comment ("Default directory has " &
         Integer'Image (Path_Segments (Default_Directory.all)) &
         " path segments");
   end if;
   if Path_Segments (Default_Directory.all) /=
      Alt_Path_Segments (Default_Directory.all) then
       Report.Failed ("Decomposing from the right gives a different answer " &
                      "than decomposing from the left");
       Report.Comment ("Default directory has " &
         Integer'Image (Alt_Path_Segments (Default_Directory.all)) &
         " path segments (left)");
   end if;

   -- Full name:
   Check_Relations (Default_Directory.all, Subtest => "Default");

   -- Relative: File1/File2

   Check_Relations (ADH.Compose (File1_Simple_Name.all, File2_Simple_Name.all),
                    Subtest => "Two simple");

   -- Relative: ../File2/File1

   Check_Relations (ADH.Compose (ADH.Compose (Impdef.Parent_Directory_Name,
                                              File2_Simple_Name.all),
                                 File1_Simple_Name.all),
                    Subtest => "Parent three");

   -- Full name: Default/File2/File1
   Check_Relations (ADH.Compose (ADH.Compose (Default_Directory.all,
                                              File2_Simple_Name.all),
                                 File1_Simple_Name.all),
                    Subtest => "Full many");

   -- Simple name:
   Check_Relations (File1_Simple_Name.all,
                    Subtest => "Simple");

   -- Current:
   Check_Relations (Impdef.Current_Directory_Name,
                    Subtest => "Current");

   -- Parent:
   Check_Relations (Impdef.Parent_Directory_Name,
                    Subtest => "Parent");

   -- Root:
   Check_Relations (ADH.Initial_Directory (Default_Directory.all),
                    Subtest => "Root");

<<Complete>>
   Report.Result;
end CXAG002;
