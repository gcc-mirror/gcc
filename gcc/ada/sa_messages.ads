------------------------------------------------------------------------------
--                       C O D E P E E R / S P A R K                        --
--                                                                          --
--                     Copyright (C) 2015-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Containers;        use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package SA_Messages is

   --  This package can be used for reading/writing a file containing a
   --  sequence of static anaysis results. Each element can describe a runtime
   --  check whose outcome has been statically determined, or it might be a
   --  warning or diagnostic message. It is expected that typically CodePeer
   --  will do the writing and SPARK will do the reading; this will allow SPARK
   --  to get the benefit of CodePeer's analysis.
   --
   --  Each item is represented as a pair consisting of a message and an
   --  associated source location. Source locations may refer to a location
   --  within the expansion of an instance of a generic; this is represented
   --  by combining the corresponding location within the generic with the
   --  location of the instance (repeated if the instance itself occurs within
   --  a generic). In addition, the type Iteration_Id is intended for use in
   --  distinguishing messages which refer to a specific iteration of a loop
   --  (this case can arise, for example, if CodePeer chooses to unroll a
   --  for-loop). This data structure is only general enough to support the
   --  kinds of unrolling that are currently planned for CodePeer. For
   --  example, an Iteration_Id can only identify an iteration of the nearest
   --  enclosing loop of the associated File/Line/Column source location.
   --  This is not a problem because CodePeer doesn't unroll loops which
   --  contain other loops.

   type Message_Kind is (

      --  Check kinds

      Array_Index_Check,
      Divide_By_Zero_Check,
      Tag_Check,
      Discriminant_Check,
      Range_Check,
      Overflow_Check,
      Assertion_Check,

      --  Warning kinds

      Suspicious_Range_Precondition_Warning,
      Suspicious_First_Precondition_Warning,
      Suspicious_Input_Warning,
      Suspicious_Constant_Operation_Warning,
      Unread_In_Out_Parameter_Warning,
      Unassigned_In_Out_Parameter_Warning,
      Non_Analyzed_Call_Warning,
      Procedure_Does_Not_Return_Warning,
      Check_Fails_On_Every_Call_Warning,
      Unknown_Call_Warning,
      Dead_Store_Warning,
      Dead_Outparam_Store_Warning,
      Potentially_Dead_Store_Warning,
      Same_Value_Dead_Store_Warning,
      Dead_Block_Warning,
      Infinite_Loop_Warning,
      Dead_Edge_Warning,
      Plain_Dead_Edge_Warning,
      True_Dead_Edge_Warning,
      False_Dead_Edge_Warning,
      True_Condition_Dead_Edge_Warning,
      False_Condition_Dead_Edge_Warning,
      Unrepeatable_While_Loop_Warning,
      Dead_Block_Continuation_Warning,
      Local_Lock_Of_Global_Object_Warning,
      Analyzed_Module_Warning,
      Non_Analyzed_Module_Warning,
      Non_Analyzed_Procedure_Warning,
      Incompletely_Analyzed_Procedure_Warning);

   --  Assertion_Check includes checks for user-defined PPCs (both specific
   --  and class-wide), Assert pragma checks, subtype predicate checks,
   --  type invariant checks (specific and class-wide), and checks for
   --  implementation-defined assertions such as Assert_And_Cut, Assume,
   --  Contract_Cases, Default_Initial_Condition, Initial_Condition,
   --  Loop_Invariant, Loop_Variant, and Refined_Post.
   --
   --  TBD: it might be nice to distinguish these different kinds of assertions
   --  as is done in SPARK's VC_Kind enumeration type, but any distinction
   --  which isn't already present in CP's BE_Message_Subkind enumeration type
   --  would require more work on the CP side.
   --
   --  The warning kinds are pretty much a copy of the set of
   --  Be_Message_Subkind values for which CP's Is_Warning predicate returns
   --  True; see descriptive comment for each in CP's message_kinds.ads .

   subtype Check_Kind is Message_Kind
     range Array_Index_Check .. Assertion_Check;
   subtype Warning_Kind is Message_Kind
     range Message_Kind'Succ (Check_Kind'Last) .. Message_Kind'Last;

   --  Possible outcomes of the static analysis of a runtime check
   --
   --  Not_Statically_Known_With_Low_Severity could be used instead of of
   --  Not_Statically_Known if there is some reason to believe that (although
   --  the tool couldn't prove it) the check is likely to always pass (in CP
   --  terms, if the corresponding CP message has severity Low as opposed to
   --  Medium). It's not clear yet whether SPARK will care about this
   --  distinction.

   type SA_Check_Result is
     (Statically_Known_Success,
      Not_Statically_Known_With_Low_Severity,
      Not_Statically_Known,
      Statically_Known_Failure);

   type SA_Message (Kind : Message_Kind := Message_Kind'Last) is record
      case Kind is
         when Check_Kind =>
            Check_Result : SA_Check_Result;

         when Warning_Kind =>
            null;
      end case;
   end record;

   type Source_Location_Or_Null (<>) is private;
   Null_Location : constant Source_Location_Or_Null;
   subtype Source_Location is Source_Location_Or_Null with
     Dynamic_Predicate => Source_Location /= Null_Location;

   type Line_Number is new Positive;
   type Column_Number is new Positive;

   function File_Name (Location : Source_Location) return String;
   function File_Name (Location : Source_Location) return Unbounded_String;
   function Line      (Location : Source_Location) return Line_Number;
   function Column    (Location : Source_Location) return Column_Number;

   type Iteration_Kind is (None, Initial, Subsequent, Numbered);
   --  None is for the usual no-unrolling case.
   --  Initial and Subsequent are for use in the case where only the first
   --  iteration of a loop (or some part thereof, such as the termination
   --  test of a while-loop) is unrolled.
   --  Numbered is for use in the case where a for-loop with a statically
   --  known number of iterations is fully unrolled.

   subtype Iteration_Number is Integer range 1 .. 255;
   subtype Iteration_Total  is Integer range 2 .. 255;

   type Iteration_Id (Kind : Iteration_Kind := None) is record
      case Kind is
         when Numbered =>
            Number   : Iteration_Number;
            Of_Total : Iteration_Total;
         when others =>
            null;
      end case;
   end record;

   function Iteration (Location : Source_Location) return Iteration_Id;

   function Enclosing_Instance
     (Location : Source_Location) return Source_Location_Or_Null;
   --  For a source location occurring within the expansion of an instance of a
   --  generic unit, the Line, Column, and File_Name selectors will indicate a
   --  location within the generic; the Enclosing_Instance selector yields the
   --  location of the declaration of the instance.

   function Make
     (File_Name : String;
      Line      : Line_Number;
      Column    : Column_Number;
      Iteration : Iteration_Id;
      Enclosing_Instance : Source_Location_Or_Null) return Source_Location;
   --  Constructor

   type Message_And_Location (<>) is private;

   function Location (Item : Message_And_Location) return Source_Location;
   function Message (Item : Message_And_Location) return SA_Message;

   function Make_Msg_Loc
     (Msg : SA_Message;
      Loc : Source_Location) return Message_And_Location;
   --  Selectors

   function "<" (Left, Right : Message_And_Location) return Boolean;
   function Hash (Key : Message_And_Location) return Hash_Type;
   --  Actuals for container instances

   File_Extension : constant String; -- ".json" (but could change in future)
   --  Clients may wish to use File_Extension in constructing
   --  File_Name parameters for calls to Open.

   package Writing is
      function Is_Open return Boolean;

      procedure Open (File_Name : String) with
        Precondition  => not Is_Open,
        Postcondition => Is_Open;
      --  Behaves like Text_IO.Create with respect to error cases

      procedure Write (Message : SA_Message; Location : Source_Location);

      procedure Close with
        Precondition  => Is_Open,
        Postcondition => not Is_Open;
      --  Behaves like Text_IO.Close with respect to error cases
   end Writing;

   package Reading is
      function Is_Open return Boolean;

      procedure Open (File_Name : String; Full_Path : Boolean := True) with
        Precondition  => not Is_Open,
        Postcondition => Is_Open;
      --  Behaves like Text_IO.Open with respect to error cases

      function Done return Boolean with
        Precondition => Is_Open;

      function Get return Message_And_Location with
        Precondition => not Done;

      procedure Close with
        Precondition  => Is_Open,
        Postcondition => not Is_Open;
      --  Behaves like Text_IO.Close with respect to error cases
   end Reading;

private
   type Simple_Source_Location is record
      File_Name : Unbounded_String := Null_Unbounded_String;
      Line      : Line_Number      := Line_Number'Last;
      Column    : Column_Number    := Column_Number'Last;
      Iteration : Iteration_Id     := (Kind => None);
   end record;

   type Source_Locations is
     array (Natural range <>) of Simple_Source_Location;

   type Source_Location_Or_Null (Count : Natural) is record
      Locations : Source_Locations (1 .. Count);
   end record;

   Null_Location : constant Source_Location_Or_Null :=
                     (Count => 0, Locations => (others => <>));

   type Message_And_Location (Count : Positive) is record
      Message  : SA_Message;
      Location : Source_Location (Count => Count);
   end record;

   File_Extension : constant String := ".json";
end SA_Messages;
