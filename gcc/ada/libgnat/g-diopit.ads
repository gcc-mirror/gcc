------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--  G N A T . D I R E C T O R Y _ O P E R A T I O N S . I T E R A T I O N   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2024, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Iterators among files

package GNAT.Directory_Operations.Iteration is

   generic
      with procedure Action
        (Item  :        String;
         Index :        Positive;
         Quit  : in out Boolean);
   procedure Find
     (Root_Directory : Dir_Name_Str;
      File_Pattern   : String);
   --  Recursively searches the directory structure rooted at Root_Directory.
   --  This provides functionality similar to the UNIX 'find' command.
   --  Action will be called for every item matching the regular expression
   --  File_Pattern (see GNAT.Regexp). Item is the full pathname to the file
   --  starting with Root_Directory that has been matched. Index is set to one
   --  for the first call and is incremented by one at each call. The iterator
   --  will pass in the value False on each call to Action. The iterator will
   --  terminate after passing the last matched path to Action or after
   --  returning from a call to Action which sets Quit to True.
   --  The iterator does not follow symbolic links avoiding possible
   --  circularities or exploring unrelated directories.
   --  Raises GNAT.Regexp.Error_In_Regexp if File_Pattern is ill formed.

   generic
      with procedure Action
        (Item  :        String;
         Index :        Positive;
         Quit  : in out Boolean);
   procedure Wildcard_Iterator (Path : Path_Name);
   --  Calls Action for each path matching Path. Path can include wildcards '*'
   --  and '?' and [...]. The rules are:
   --
   --     *       can be replaced by any sequence of characters
   --     ?       can be replaced by a single character
   --     [a-z]   match one character in the range 'a' through 'z'
   --     [abc]   match either character 'a', 'b' or 'c'
   --
   --  Item is the filename that has been matched. Index is set to one for the
   --  first call and is incremented by one at each call. The iterator's
   --  termination can be controlled by setting Quit to True. It is by default
   --  set to False.
   --
   --  For example, if we have the following directory structure:
   --     /boo/
   --        foo.ads
   --     /sed/
   --        foo.ads
   --        file/
   --          foo.ads
   --     /sid/
   --        foo.ads
   --        file/
   --          foo.ads
   --     /life/
   --
   --  A call with expression "/s*/file/*" will call Action for the following
   --  items:
   --     /sed/file/foo.ads
   --     /sid/file/foo.ads

end GNAT.Directory_Operations.Iteration;
