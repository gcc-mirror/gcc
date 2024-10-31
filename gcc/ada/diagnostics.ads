------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          D I A G N O S T I C S                           --
--                                                                          --
--                                 S p e c                                  --
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

with Types;      use Types;
with GNAT.Lists; use GNAT.Lists;

package Diagnostics is

   type Diagnostic_Id is
     (No_Diagnostic_Id,
      GNAT0001,
      GNAT0002,
      GNAT0003,
      GNAT0004,
      GNAT0005,
      GNAT0006,
      GNAT0007,
      GNAT0008,
      GNAT0009,
      GNAT0010,
      GNAT0011);

   --  Labeled_Span_Type represents a span of source code that is associated
   --  with a textual label. Primary spans indicate the primary location of the
   --  diagnostic. Non-primary spans are used to indicate secondary locations.
   --
   --  Spans can contain labels that are used to annotate the highlighted span.
   --  Usually, the label is a short and concise message that provide
   --  additional allthough non-critical information about the span. This is
   --  an important since labels are not printed in the brief output and are
   --  only present in the pretty and structural outputs. That is an important
   --  distintion when choosing between a label and a sub-diagnostic.
   type Labeled_Span_Type is record
      Label : String_Ptr := null;
      --  Text associated with the span

      Span : Source_Span := (others => No_Location);
      --  Textual region in the source code

      Is_Primary : Boolean := True;
      --  Primary spans are used to indicate the primary location of the
      --  diagnostic. Typically there should just be one primary span per
      --  diagnostic.
      --  Non-primary spans are used to indicate secondary locations and
      --  typically are formatted in a different way or omitted in some
      --  contexts.

      Is_Region : Boolean := False;
      --  Regional spans are multiline spans that have a unique way of being
      --  displayed in the pretty output.
   end record;

   No_Labeled_Span : constant Labeled_Span_Type := (others => <>);

   procedure Destroy (Elem : in out Labeled_Span_Type);
   pragma Inline (Destroy);

   package Labeled_Span_Lists is new Doubly_Linked_Lists
     (Element_Type    => Labeled_Span_Type,
      "="             => "=",
      Destroy_Element => Destroy,
      Check_Tampering => False);
   subtype Labeled_Span_List is Labeled_Span_Lists.Doubly_Linked_List;

   type Sub_Diagnostic_Kind is
     (Continuation,
      Help,
      Note,
      Suggestion);

   --  Sub_Diagnostic_Type represents a sub-diagnostic message that is meant
   --  to provide additional information about the primary diagnostic message.
   --
   --  Sub-diagnostics are usually constructed with a full sentence as the
   --  message and provide important context to the main diagnostic message or
   --  some concrete action to the user.
   --
   --  This is different from the labels of labeled spans which are meant to be
   --  short and concise and are mostly there to annotate the higlighted span.

   type Sub_Diagnostic_Type is record
      Kind : Sub_Diagnostic_Kind;

      Message : String_Ptr;

      Locations : Labeled_Span_List;
   end record;

   procedure Add_Location
     (Diagnostic : in out Sub_Diagnostic_Type; Location : Labeled_Span_Type);

   function Primary_Location
     (Diagnostic : Sub_Diagnostic_Type) return Labeled_Span_Type;

   procedure Destroy (Elem : in out Sub_Diagnostic_Type);
   pragma Inline (Destroy);

   package Sub_Diagnostic_Lists is new Doubly_Linked_Lists
     (Element_Type    => Sub_Diagnostic_Type,
      "="             => "=",
      Destroy_Element => Destroy,
      Check_Tampering => False);

   subtype Sub_Diagnostic_List is Sub_Diagnostic_Lists.Doubly_Linked_List;

   --  An Edit_Type represents a textual edit that is associated with a Fix.
   type Edit_Type is record
      Span : Source_Span;
      --  Region of the file to be removed

      Text : String_Ptr;
      --  Text to be inserted at the start location of the span
   end record;

   procedure Destroy (Elem : in out Edit_Type);
   pragma Inline (Destroy);

   package Edit_Lists is new Doubly_Linked_Lists
     (Element_Type    => Edit_Type,
      "="             => "=",
      Destroy_Element => Destroy,
      Check_Tampering => False);

   subtype Edit_List is Edit_Lists.Doubly_Linked_List;

   --  Type Applicability_Type will indicate the state of the resulting code
   --  after applying a fix.
   --  * Option Has_Placeholders indicates that the fix contains placeholders
   --    that the user would need to fill.
   --  * Option Legal indicates that applying the fix will result in legal Ada
   --    code.
   --  * Option Possibly_Illegal indicates that applying the fix will result in
   --    possibly legal, but also possibly illegal Ada code.
   type Applicability_Type is
     (Has_Placeholders,
      Legal,
      Possibly_Illegal,
      Unspecified);

   type Fix_Type is record
      Description : String_Ptr := null;
      --  Message describing the fix that will be displayed to the user.

      Applicability : Applicability_Type := Unspecified;

      Edits : Edit_List;
      --   File changes for the fix.
   end record;

   procedure Destroy (Elem : in out Fix_Type);
   pragma Inline (Destroy);

   package Fix_Lists is new Doubly_Linked_Lists
     (Element_Type    => Fix_Type,
      "="             => "=",
      Destroy_Element => Destroy,
      Check_Tampering => False);

   subtype Fix_List is Fix_Lists.Doubly_Linked_List;

   procedure Add_Edit (Fix : in out Fix_Type; Edit : Edit_Type);

   type Status_Type is
     (Active,
      Deprecated);

   type Switch_Id is (
      No_Switch_Id,
      gnatwb,
      gnatwc,
      gnatwd,
      gnatwf,
      gnatwg,
      gnatwh,
      gnatwi,
      gnatwj,
      gnatwk,
      gnatwl,
      gnatwm,
      gnatwo,
      gnatwp,
      gnatwq,
      gnatwr,
      gnatwt,
      gnatwu,
      gnatwv,
      gnatww,
      gnatwx,
      gnatwy,
      gnatwz,
      gnatw_dot_a,
      gnatw_dot_b,
      gnatw_dot_c,
      gnatw_dot_f,
      gnatw_dot_h,
      gnatw_dot_i,
      gnatw_dot_j,
      gnatw_dot_k,
      gnatw_dot_l,
      gnatw_dot_m,
      gnatw_dot_n,
      gnatw_dot_o,
      gnatw_dot_p,
      gnatw_dot_q,
      gnatw_dot_r,
      gnatw_dot_s,
      gnatw_dot_t,
      gnatw_dot_u,
      gnatw_dot_v,
      gnatw_dot_w,
      gnatw_dot_x,
      gnatw_dot_y,
      gnatw_dot_z,
      gnatw_underscore_a,
      gnatw_underscore_c,
      gnatw_underscore_j,
      gnatw_underscore_l,
      gnatw_underscore_p,
      gnatw_underscore_q,
      gnatw_underscore_r,
      gnatw_underscore_s,
      gnaty,
      gnatya,
      gnatyb,
      gnatyc,
      gnatyd,
      gnatye,
      gnatyf,
      gnatyh,
      gnatyi,
      gnatyk,
      gnatyl,
      gnatym,
      gnatyn,
      gnatyo,
      gnatyp,
      gnatyr,
      gnatys,
      gnatyu,
      gnatyx,
      gnatyz,
      gnatyaa,
      gnatybb,
      gnatycc,
      gnatydd,
      gnatyii,
      gnatyll,
      gnatymm,
      gnatyoo,
      gnatyss,
      gnatytt,
      gnatel
   );

   subtype Active_Switch_Id is Switch_Id range gnatwb .. gnatel;
   --  The range of switch ids that represent switches that trigger a specific
   --  diagnostic check.

   type Switch_Type is record

      Status : Status_Type := Active;
      --  The status will indicate whether the switch is currently active,
      --  or has been deprecated. A deprecated switch will not control
      --  diagnostics, and will not be emitted by the GNAT usage.

      Human_Id : String_Ptr := null;
      --  The Human_Id will be a unique and stable string-based ID which
      --  identifies the content of the switch within the switch registry.
      --  This ID will appear in SARIF readers.

      Short_Name : String_Ptr := null;
      --  The Short_Name will denote the -gnatXX name of the switch.

      Description : String_Ptr := null;
      --  The description will contain the description of the switch, as it is
      --  currently emitted by the GNAT usage.

      Documentation_Url : String_Ptr := null;
      --  The documentation_url will point to the AdaCore documentation site
      --  for the switch.

   end record;

   type Diagnostic_Kind is
     (Error,
      Non_Serious_Error,
      --  Typically all errors are considered serious and the compiler should
      --  stop its processing since the tree is essentially invalid. However,
      --  some errors are not serious and the compiler can continue its
      --  processing to discover more critical errors.
      Warning,
      Default_Warning,
      --  Warning representing the old warnings created with the '??' insertion
      --  character. These warning have the [enabled by default] tag.
      Restriction_Warning,
      --  Warning representing the old warnings created with the '?*?'
      --  insertion character. These warning have the [restriction warning]
      --  tag.
      Style,
      Tagless_Warning,
      --  Warning representing the old warnings created with the '?' insertion
      --  character.
      Info
   );

   type Diagnostic_Entry_Type is record
      Status : Status_Type := Active;

      Human_Id : String_Ptr := null;
      --  A human readable code for the diagnostic. If the diagnostic has a
      --  switch with a human id then the human_id of the switch shall be used
      --  in SARIF reports.

      Documentation : String_Ptr := null;

      Switch : Switch_Id := No_Switch_Id;
      --  The switch that controls the diagnostic message.
   end record;

   type Diagnostic_Type is record

      Id : Diagnostic_Id := No_Diagnostic_Id;

      Kind : Diagnostic_Kind := Error;

      Switch : Switch_Id := No_Switch_Id;

      Message : String_Ptr := null;

      Warn_Err : Boolean := False;
      --  Signal whether the diagnostic was converted from a warning to an
      --  error. This needs to be set during the message emission as this
      --  behavior depends on the context of the code.

      Locations : Labeled_Span_List := Labeled_Span_Lists.Nil;

      Sub_Diagnostics : Sub_Diagnostic_List := Sub_Diagnostic_Lists.Nil;

      Fixes : Fix_List := Fix_Lists.Nil;
   end record;

   procedure Destroy (Elem : in out Diagnostic_Type);
   pragma Inline (Destroy);

   package Diagnostics_Lists is new Doubly_Linked_Lists
     (Element_Type    => Diagnostic_Type,
      "="             => "=",
      Destroy_Element => Destroy,
      Check_Tampering => False);

   subtype Diagnostic_List is Diagnostics_Lists.Doubly_Linked_List;

   All_Diagnostics : Diagnostic_List := Diagnostics_Lists.Create;

   procedure Add_Location
     (Diagnostic : in out Diagnostic_Type; Location : Labeled_Span_Type);

   procedure Add_Sub_Diagnostic
     (Diagnostic     : in out Diagnostic_Type;
      Sub_Diagnostic :        Sub_Diagnostic_Type);

   procedure Add_Fix (Diagnostic : in out Diagnostic_Type; Fix : Fix_Type);

   procedure Record_Diagnostic (Diagnostic : Diagnostic_Type;
                                Update_Count : Boolean := True);

   procedure Print_Diagnostic (Diagnostic : Diagnostic_Type);

   function Primary_Location
     (Diagnostic : Diagnostic_Type) return Labeled_Span_Type;

   type Labeled_Span_Array is
     array (Positive range <>) of Labeled_Span_Type;
   type Sub_Diagnostic_Array is
     array (Positive range <>) of Sub_Diagnostic_Type;
   type Fix_Array is
     array (Positive range <>) of Fix_Type;
   type Edit_Array is
     array (Positive range <>) of Edit_Type;

   No_Locations : constant Labeled_Span_Array (1 .. 0)   := (others => <>);
   No_Sub_Diags : constant Sub_Diagnostic_Array (1 .. 0) := (others => <>);
   No_Fixes     : constant Fix_Array (1 .. 0)            := (others => <>);
   No_Edits     : constant Edit_Array (1 .. 0)           := (others => <>);

   function Make_Diagnostic
     (Msg       : String;
      Location  : Labeled_Span_Type;
      Id        : Diagnostic_Id        := No_Diagnostic_Id;
      Kind      : Diagnostic_Kind      := Diagnostics.Error;
      Switch    : Switch_Id            := No_Switch_Id;
      Spans     : Labeled_Span_Array   := No_Locations;
      Sub_Diags : Sub_Diagnostic_Array := No_Sub_Diags;
      Fixes     : Fix_Array            := No_Fixes)
      return Diagnostic_Type;

   procedure Record_Diagnostic
     (Msg       : String;
      Location  : Labeled_Span_Type;
      Id        : Diagnostic_Id        := No_Diagnostic_Id;
      Kind      : Diagnostic_Kind      := Diagnostics.Error;
      Switch    : Switch_Id            := No_Switch_Id;
      Spans     : Labeled_Span_Array   := No_Locations;
      Sub_Diags : Sub_Diagnostic_Array := No_Sub_Diags;
      Fixes     : Fix_Array            := No_Fixes);

   function Labeled_Span (Span       : Source_Span;
                          Label      : String  := "";
                          Is_Primary : Boolean := True;
                          Is_Region  : Boolean := False)
                          return Labeled_Span_Type;

   function Primary_Labeled_Span (Span  : Source_Span;
                                  Label : String  := "")
                                  return Labeled_Span_Type;

   function Primary_Labeled_Span (N     : Node_Or_Entity_Id;
                                  Label : String  := "")
                                  return Labeled_Span_Type;

   function Secondary_Labeled_Span (Span  : Source_Span;
                                    Label : String  := "")
                                    return Labeled_Span_Type;

   function Secondary_Labeled_Span (N     : Node_Or_Entity_Id;
                                    Label : String  := "")
                                    return Labeled_Span_Type;

   function Sub_Diag (Msg       : String;
                      Kind      : Sub_Diagnostic_Kind :=
                        Diagnostics.Continuation;
                      Locations : Labeled_Span_Array := No_Locations)
                      return Sub_Diagnostic_Type;

   function Continuation (Msg       : String;
                          Locations : Labeled_Span_Array := No_Locations)
                          return Sub_Diagnostic_Type;

   function Help (Msg       : String;
                  Locations : Labeled_Span_Array := No_Locations)
                  return Sub_Diagnostic_Type;

   function Suggestion (Msg       : String;
                        Locations : Labeled_Span_Array := No_Locations)
                        return Sub_Diagnostic_Type;

   function Fix (Description   : String;
                 Edits         : Edit_Array;
                 Applicability : Applicability_Type := Unspecified)
                 return Fix_Type;

   function Edit (Text : String;
                  Span : Source_Span)
                  return Edit_Type;
end Diagnostics;
