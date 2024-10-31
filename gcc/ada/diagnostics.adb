------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          D I A G N O S T I C S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;                  use Atree;
with Debug;                  use Debug;
with Diagnostics.Brief_Emitter;
with Diagnostics.Pretty_Emitter;
with Diagnostics.Repository; use Diagnostics.Repository;
with Diagnostics.Utils;      use Diagnostics.Utils;
with Lib;                    use Lib;
with Opt;                    use Opt;
with Sinput;                 use Sinput;
with Warnsw;

package body Diagnostics is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out Labeled_Span_Type) is
   begin
      Free (Elem.Label);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out Sub_Diagnostic_Type) is
   begin
      Free (Elem.Message);
      if Labeled_Span_Lists.Present (Elem.Locations) then
         Labeled_Span_Lists.Destroy (Elem.Locations);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out Edit_Type) is
   begin
      Free (Elem.Text);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out Fix_Type) is
   begin
      Free (Elem.Description);
      if Edit_Lists.Present (Elem.Edits) then
         Edit_Lists.Destroy (Elem.Edits);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Elem : in out Diagnostic_Type) is
   begin
      Free (Elem.Message);
      if Labeled_Span_Lists.Present (Elem.Locations) then
         Labeled_Span_Lists.Destroy (Elem.Locations);
      end if;
      if Sub_Diagnostic_Lists.Present (Elem.Sub_Diagnostics) then
         Sub_Diagnostic_Lists.Destroy (Elem.Sub_Diagnostics);
      end if;
      if Fix_Lists.Present (Elem.Fixes) then
         Fix_Lists.Destroy (Elem.Fixes);
      end if;
   end Destroy;

   ------------------
   -- Add_Location --
   ------------------

   procedure Add_Location
     (Diagnostic : in out Sub_Diagnostic_Type; Location : Labeled_Span_Type)
   is
      use Labeled_Span_Lists;
   begin
      if not Present (Diagnostic.Locations) then
         Diagnostic.Locations := Create;
      end if;

      Append (Diagnostic.Locations, Location);
   end Add_Location;

   ----------------------
   -- Primary_Location --
   ----------------------

   function Primary_Location
     (Diagnostic : Sub_Diagnostic_Type) return Labeled_Span_Type
   is
   begin
      return Get_Primary_Labeled_Span (Diagnostic.Locations);
   end Primary_Location;

   ------------------
   -- Add_Location --
   ------------------

   procedure Add_Location
     (Diagnostic : in out Diagnostic_Type; Location : Labeled_Span_Type)
   is
      use Labeled_Span_Lists;
   begin
      if not Present (Diagnostic.Locations) then
         Diagnostic.Locations := Create;
      end if;

      Append (Diagnostic.Locations, Location);
   end Add_Location;

   ------------------------
   -- Add_Sub_Diagnostic --
   ------------------------

   procedure Add_Sub_Diagnostic
     (Diagnostic     : in out Diagnostic_Type;
      Sub_Diagnostic :        Sub_Diagnostic_Type)
   is
      use Sub_Diagnostic_Lists;
   begin
      if not Present (Diagnostic.Sub_Diagnostics) then
         Diagnostic.Sub_Diagnostics := Create;
      end if;

      Append (Diagnostic.Sub_Diagnostics, Sub_Diagnostic);
   end Add_Sub_Diagnostic;

   procedure Add_Edit (Fix : in out Fix_Type; Edit : Edit_Type) is
      use Edit_Lists;
   begin
      if not Present (Fix.Edits) then
         Fix.Edits := Create;
      end if;

      Append (Fix.Edits, Edit);
   end Add_Edit;

   -------------
   -- Add_Fix --
   -------------

   procedure Add_Fix (Diagnostic : in out Diagnostic_Type; Fix : Fix_Type) is
      use Fix_Lists;
   begin
      if not Present (Diagnostic.Fixes) then
         Diagnostic.Fixes := Create;
      end if;

      Append (Diagnostic.Fixes, Fix);
   end Add_Fix;

   -----------------------
   -- Record_Diagnostic --
   -----------------------

   procedure Record_Diagnostic (Diagnostic : Diagnostic_Type;
                                Update_Count : Boolean := True)
   is

      procedure Update_Diagnostic_Count (Diagnostic : Diagnostic_Type);

      -----------------------------
      -- Update_Diagnostic_Count --
      -----------------------------

      procedure Update_Diagnostic_Count (Diagnostic : Diagnostic_Type) is

      begin
         case Diagnostic.Kind is
            when Error =>
               Total_Errors_Detected := Total_Errors_Detected + 1;
               Serious_Errors_Detected := Serious_Errors_Detected + 1;

            when Non_Serious_Error =>
               Total_Errors_Detected := Total_Errors_Detected + 1;

            when Warning
               | Default_Warning
               | Tagless_Warning
               | Restriction_Warning
               | Style
            =>
               Warnings_Detected := Warnings_Detected + 1;

               if Diagnostic.Warn_Err then
                  Warnings_Treated_As_Errors := Warnings_Treated_As_Errors + 1;
               end if;

            when Info =>
               Info_Messages := Info_Messages + 1;
         end case;
      end Update_Diagnostic_Count;

      procedure Handle_Serious_Error;
      --  Internal procedure to do all error message handling for a serious
      --  error message, other than bumping the error counts and arranging
      --  for the message to be output.

      procedure Handle_Serious_Error is
      begin
         --  Turn off code generation if not done already

         if Operating_Mode = Generate_Code then
            Operating_Mode := Check_Semantics;
            Expander_Active := False;
         end if;

         --  Set the fatal error flag in the unit table unless we are in
         --  Try_Semantics mode (in which case we set ignored mode if not
         --  currently set. This stops the semantics from being performed
         --  if we find a serious error. This is skipped if we are currently
         --  dealing with the configuration pragma file.

         if Current_Source_Unit /= No_Unit then
            declare
               U : constant Unit_Number_Type :=
                 Get_Source_Unit
                   (Primary_Location (Diagnostic).Span.Ptr);
            begin
               if Try_Semantics then
                  if Fatal_Error (U) = None then
                     Set_Fatal_Error (U, Error_Ignored);
                  end if;
               else
                  Set_Fatal_Error (U, Error_Detected);
               end if;
            end;
         end if;

         --  Disable warnings on unused use clauses and the like. Otherwise, an
         --  error might hide a reference to an entity in a used package, so
         --  after fixing the error, the use clause no longer looks like it was
         --  unused.

         Warnsw.Check_Unreferenced := False;
         Warnsw.Check_Unreferenced_Formals := False;
      end Handle_Serious_Error;
   begin
      Insert_Based_On_Location (All_Diagnostics, Diagnostic);

      if Update_Count then
         Update_Diagnostic_Count (Diagnostic);
      end if;

      if Diagnostic.Kind = Error then
         Handle_Serious_Error;
      end if;
   end Record_Diagnostic;

   ----------------------
   -- Print_Diagnostic --
   ----------------------

   procedure Print_Diagnostic (Diagnostic : Diagnostic_Type) is

   begin
      if Debug_Flag_FF then
         Diagnostics.Pretty_Emitter.Print_Diagnostic (Diagnostic);
      else
         Diagnostics.Brief_Emitter.Print_Diagnostic (Diagnostic);
      end if;
   end Print_Diagnostic;

   ----------------------
   -- Primary_Location --
   ----------------------

   function Primary_Location
     (Diagnostic : Diagnostic_Type) return Labeled_Span_Type
   is
   begin
      return Get_Primary_Labeled_Span (Diagnostic.Locations);
   end Primary_Location;

   ---------------------
   -- Make_Diagnostic --
   ---------------------

   function Make_Diagnostic
     (Msg       : String;
      Location  : Labeled_Span_Type;
      Id        : Diagnostic_Id        := No_Diagnostic_Id;
      Kind      : Diagnostic_Kind      := Diagnostics.Error;
      Switch    : Switch_Id            := No_Switch_Id;
      Spans     : Labeled_Span_Array   := No_Locations;
      Sub_Diags : Sub_Diagnostic_Array := No_Sub_Diags;
      Fixes     : Fix_Array            := No_Fixes)
      return Diagnostic_Type
   is
      D : Diagnostic_Type;
   begin
      D.Message := new String'(Msg);
      D.Id      := Id;
      D.Kind    := Kind;

      if Id /= No_Diagnostic_Id then
         pragma Assert (Switch = Diagnostic_Entries (Id).Switch,
            "Provided switch must be the same as in the registry");
      end if;
      D.Switch  := Switch;

      pragma Assert (Location.Is_Primary, "Main location must be primary");
      Add_Location (D, Location);

      for I in Spans'Range loop
         Add_Location (D, Spans (I));
      end loop;

      for I in Sub_Diags'Range loop
         Add_Sub_Diagnostic (D, Sub_Diags (I));
      end loop;

      for I in Fixes'Range loop
         Add_Fix (D, Fixes (I));
      end loop;

      return D;
   end Make_Diagnostic;

   -----------------------
   -- Record_Diagnostic --
   -----------------------

   procedure Record_Diagnostic
     (Msg       : String;
      Location  : Labeled_Span_Type;
      Id        : Diagnostic_Id        := No_Diagnostic_Id;
      Kind      : Diagnostic_Kind      := Diagnostics.Error;
      Switch    : Switch_Id            := No_Switch_Id;
      Spans     : Labeled_Span_Array   := No_Locations;
      Sub_Diags : Sub_Diagnostic_Array := No_Sub_Diags;
      Fixes     : Fix_Array            := No_Fixes)
   is

   begin
      Record_Diagnostic
        (Make_Diagnostic
           (Msg       => Msg,
            Location  => Location,
            Id        => Id,
            Kind      => Kind,
            Switch    => Switch,
            Spans     => Spans,
            Sub_Diags => Sub_Diags,
            Fixes     => Fixes));
   end Record_Diagnostic;

   ------------------
   -- Labeled_Span --
   ------------------

   function Labeled_Span (Span       : Source_Span;
                          Label      : String := "";
                          Is_Primary : Boolean := True;
                          Is_Region  : Boolean := False)
                          return Labeled_Span_Type
   is
      L : Labeled_Span_Type;
   begin
      L.Span       := Span;
      if Label /= "" then
         L.Label      := new String'(Label);
      end if;
      L.Is_Primary := Is_Primary;
      L.Is_Region  := Is_Region;

      return L;
   end Labeled_Span;

   --------------------------
   -- Primary_Labeled_Span --
   --------------------------

   function Primary_Labeled_Span (Span  : Source_Span;
                                  Label : String  := "")
                                  return Labeled_Span_Type
   is begin
      return Labeled_Span (Span => Span, Label => Label, Is_Primary => True);
   end Primary_Labeled_Span;

   --------------------------
   -- Primary_Labeled_Span --
   --------------------------

   function Primary_Labeled_Span (N     : Node_Or_Entity_Id;
                                  Label : String := "")
                                  return Labeled_Span_Type
   is
   begin
      return Primary_Labeled_Span (To_Full_Span (N), Label);
   end Primary_Labeled_Span;

   ----------------------------
   -- Secondary_Labeled_Span --
   ----------------------------

   function Secondary_Labeled_Span
     (Span  : Source_Span;
      Label : String := "")
      return Labeled_Span_Type
   is
   begin
      return Labeled_Span (Span => Span, Label => Label, Is_Primary => False);
   end Secondary_Labeled_Span;

   ----------------------------
   -- Secondary_Labeled_Span --
   ----------------------------

   function Secondary_Labeled_Span (N     : Node_Or_Entity_Id;
                                    Label : String  := "")
                                    return Labeled_Span_Type
   is
   begin
      return Secondary_Labeled_Span (To_Full_Span (N), Label);
   end Secondary_Labeled_Span;

   --------------
   -- Sub_Diag --
   --------------

   function Sub_Diag (Msg       : String;
                      Kind      : Sub_Diagnostic_Kind :=
                        Diagnostics.Continuation;
                      Locations : Labeled_Span_Array  := No_Locations)
                      return Sub_Diagnostic_Type
   is
      S : Sub_Diagnostic_Type;
   begin
      S.Message := new String'(Msg);
      S.Kind    := Kind;

      for I in Locations'Range loop
         Add_Location (S, Locations (I));
      end loop;

      return S;
   end Sub_Diag;

   ------------------
   -- Continuation --
   ------------------

   function Continuation (Msg       : String;
                          Locations : Labeled_Span_Array := No_Locations)
                          return Sub_Diagnostic_Type
   is
   begin
      return Sub_Diag (Msg, Diagnostics.Continuation, Locations);
   end Continuation;

   ----------
   -- Help --
   ----------

   function Help (Msg       : String;
                  Locations : Labeled_Span_Array := No_Locations)
                  return Sub_Diagnostic_Type
   is
   begin
      return Sub_Diag (Msg, Diagnostics.Help, Locations);
   end Help;

   ----------------
   -- Suggestion --
   ----------------

   function Suggestion (Msg       : String;
                        Locations : Labeled_Span_Array := No_Locations)
                        return Sub_Diagnostic_Type
   is
   begin
      return Sub_Diag (Msg, Diagnostics.Suggestion, Locations);
   end Suggestion;

   ---------
   -- Fix --
   ---------

   function Fix
     (Description   : String;
      Edits         : Edit_Array;
      Applicability : Applicability_Type := Unspecified) return Fix_Type
   is
      F : Fix_Type;
   begin
      F.Description   := new String'(Description);

      for I in Edits'Range loop
         Add_Edit (F, Edits (I));
      end loop;

      F.Applicability := Applicability;

      return F;
   end Fix;

   ----------
   -- Edit --
   ----------

   function Edit (Text : String; Span : Source_Span) return Edit_Type is

   begin
      return (Text => new String'(Text), Span => Span);
   end Edit;

end Diagnostics;
