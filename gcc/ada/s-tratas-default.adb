------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T R A C E S . T A S K I N G                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--           Copyright (C) 2001-2004 Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Tasking;       use System.Tasking;
with System.Soft_Links;
with System.Parameters;
with System.Traces.Format; use System.Traces.Format;
with System.Traces;        use System.Traces;

package body System.Traces.Tasking is

   use System.Tasking;
   use System.Traces;
   use System.Traces.Format;

   package SSL renames System.Soft_Links;

   function Extract_Accepts (Task_Name : Task_Id) return String_Trace;
   --  This function is used to extract data joined with
   --  W_Select, WT_Select, W_Accept events

   ---------------------
   -- Send_Trace_Info --
   ---------------------

   procedure Send_Trace_Info (Id : Trace_T; Task_Name2 : Task_Id) is
      Task_S  : constant String := SSL.Task_Name.all;
      Task2_S : constant String :=
                  Task_Name2.Common.Task_Image
                    (1 .. Task_Name2.Common.Task_Image_Len);
      Trace_S : String (1 .. 6 + Task_S'Length + Task2_S'Length);

      L0 : Integer := Task_S'Length;
      L1 : Integer := Task2_S'Length;

   begin
      if Parameters.Runtime_Traces then
         case Id is
            when M_RDV_Complete | PO_Done =>
               Trace_S (1 .. 3)                 := "/N:";
               Trace_S (4 .. 3 + L0)            := Task_S;
               Trace_S (4 + L0 .. 6 + L0)       := "/C:";
               Trace_S (7 + L0 .. Trace_S'Last) := Task2_S;
               Send_Trace (Id, Trace_S);

            when E_Missed =>
               Trace_S (1 .. 3)                 := "/N:";
               Trace_S (4 .. 3 + L0)            := Task_S;
               Trace_S (4 + L0 .. 6 + L0)       := "/A:";
               Trace_S (7 + L0 .. Trace_S'Last) := Task2_S;
               Send_Trace (Id, Trace_S);

            when E_Kill =>
               Trace_S (1 .. 3)                 := "/N:";
               Trace_S (4 .. 3 + L1)            := Task2_S;
               Trace_S (4 + L1 .. Trace_S'Last) := (others => ' ');
               Send_Trace (Id, Trace_S);

            when T_Create =>
               Trace_S (1 .. 3)                 := "/N:";
               Trace_S (4 .. 3 + L1)            := Task2_S;
               Trace_S (4 + L1 .. Trace_S'Last) := (others => ' ');
               Send_Trace (Id, Trace_S);

            when others =>
               null;
               --  should raise an exception ???
         end case;
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Task_Name2   : Task_Id;
      Entry_Number : Entry_Index)
   is
      Task_S  : constant String := SSL.Task_Name.all;
      Task2_S : constant String :=
                  Task_Name2.Common.Task_Image
                    (1 .. Task_Name2.Common.Task_Image_Len);
      Entry_S   : String := Integer'Image (Integer (Entry_Number));
      Trace_S   : String (1 .. 9 + Task_S'Length
                                 + Task2_S'Length + Entry_S'Length);

      L0 : Integer := Task_S'Length;
      L1 : Integer := Task_S'Length + Entry_S'Length;
      L2 : Integer := Task_S'Length + Task2_S'Length;

   begin
      if Parameters.Runtime_Traces then
         case Id is
            when M_Accept_Complete =>
               Trace_S (1 .. 3)                  := "/N:";
               Trace_S (4 .. 3 + L0)             := Task_S;
               Trace_S (4 + L0 .. 6 + L0)        := "/E:";
               Trace_S (7 + L0 .. 6 + L1)         := Entry_S;
               Trace_S (7 + L1 .. 9 + L1)        := "/C:";
               Trace_S (10 + L1 .. Trace_S'Last) := Task2_S;
               Send_Trace (Id, Trace_S);

            when W_Call =>
               Trace_S (1 .. 3)                  := "/N:";
               Trace_S (4 .. 3 + L0)             := Task_S;
               Trace_S (4 + L0 .. 6 + L0)        := "/A:";
               Trace_S (7 + L0 .. 6 + L2)        := Task2_S;
               Trace_S (7 + L2 .. 9 + L2)        := "/C:";
               Trace_S (10 + L2 .. Trace_S'Last) := Entry_S;
               Send_Trace (Id, Trace_S);

            when others =>
               null;
               --  should raise an exception ???
         end case;
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Task_Name    : Task_Id;
      Task_Name2   : Task_Id;
      Entry_Number : Entry_Index)
   is
      Task_S  : constant String :=
                  Task_Name.Common.Task_Image
                    (1 .. Task_Name.Common.Task_Image_Len);
      Task2_S : constant String :=
                  Task_Name2.Common.Task_Image
                    (1 .. Task_Name2.Common.Task_Image_Len);
      Entry_S   : String := Integer'Image (Integer (Entry_Number));
      Trace_S   : String (1 .. 9 + Task_S'Length
                                 + Task2_S'Length + Entry_S'Length);

      L0 : Integer := Task_S'Length;
      L1 : Integer := Task_S'Length + Entry_S'Length;

   begin
      if Parameters.Runtime_Traces then
         case Id is
            when PO_Run =>
               Trace_S (1 .. 3)                  := "/N:";
               Trace_S (4 .. 3 + L0)             := Task_S;
               Trace_S (4 + L0 .. 6 + L0)        := "/E:";
               Trace_S (7 + L0 .. 6 + L1)        := Entry_S;
               Trace_S (7 + L1 .. 9 + L1)        := "/C:";
               Trace_S (10 + L1 .. Trace_S'Last) := Task2_S;
               Send_Trace (Id, Trace_S);

            when others =>
               null;
               --  should raise an exception ???
         end case;
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info (Id : Trace_T; Entry_Number : Entry_Index) is
      Task_S  : String := SSL.Task_Name.all;
      Entry_S : String := Integer'Image (Integer (Entry_Number));
      Trace_S : String (1 .. 6 + Task_S'Length + Entry_S'Length);

      L0 : Integer := Task_S'Length;

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3)                 := "/N:";
         Trace_S (4 .. 3 + L0)            := Task_S;
         Trace_S (4 + L0 .. 6 + L0)       := "/E:";
         Trace_S (7 + L0 .. Trace_S'Last) := Entry_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info
     (Id         : Trace_T;
      Task_Name  : Task_Id;
      Task_Name2 : Task_Id)
   is
      Task_S  : constant String :=
                  Task_Name.Common.Task_Image
                    (1 .. Task_Name.Common.Task_Image_Len);
      Task2_S : constant String :=
                  Task_Name2.Common.Task_Image
                    (1 .. Task_Name2.Common.Task_Image_Len);
      Trace_S : String (1 .. 6 + Task_S'Length + Task2_S'Length);

      L0 : Integer := Task2_S'Length;

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3)                 := "/N:";
         Trace_S (4 .. 3 + L0)            := Task2_S;
         Trace_S (4 + L0 .. 6 + L0)       := "/P:";
         Trace_S (7 + L0 .. Trace_S'Last) := Task_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Acceptor     : Task_Id;
      Entry_Number : Entry_Index;
      Timeout      : Duration)
   is
      Task_S     : constant String := SSL.Task_Name.all;
      Acceptor_S : constant String :=
                     Acceptor.Common.Task_Image
                       (1 .. Acceptor.Common.Task_Image_Len);
      Entry_S    : String := Integer'Image (Integer (Entry_Number));
      Timeout_S  : String := Duration'Image (Timeout);
      Trace_S    : String (1 .. 12 + Task_S'Length + Acceptor_S'Length
                                   + Entry_S'Length + Timeout_S'Length);

      L0 : Integer := Task_S'Length;
      L1 : Integer := Task_S'Length + Acceptor_S'Length;
      L2 : Integer := Task_S'Length + Acceptor_S'Length + Entry_S'Length;

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3)                  := "/N:";
         Trace_S (4 .. 3 + L0)             := Task_S;
         Trace_S (4 + L0 .. 6 + L0)        := "/A:";
         Trace_S (7 + L0 .. 6 + L1)        := Acceptor_S;
         Trace_S (7 + L1 .. 9 + L1)        := "/E:";
         Trace_S (10 + L1 .. 9 + L2)       := Entry_S;
         Trace_S (10 + L2 .. 12 + L2)      := "/T:";
         Trace_S (13 + L2 .. Trace_S'Last) := Timeout_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info
     (Id           : Trace_T;
      Entry_Number : Entry_Index;
      Timeout      : Duration)
   is
      Task_S    : String := SSL.Task_Name.all;
      Entry_S   : String := Integer'Image (Integer (Entry_Number));
      Timeout_S : String := Duration'Image (Timeout);
      Trace_S   : String (1 .. 9 + Task_S'Length
                                 + Entry_S'Length + Timeout_S'Length);

      L0 : Integer := Task_S'Length;
      L1 : Integer := Task_S'Length + Entry_S'Length;

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3)                  := "/N:";
         Trace_S (4 .. 3 + L0)             := Task_S;
         Trace_S (4 + L0 .. 6 + L0)        := "/E:";
         Trace_S (7 + L0 .. 6 + L1)        := Entry_S;
         Trace_S (7 + L1 .. 9 + L1)        := "/T:";
         Trace_S (10 + L1 .. Trace_S'Last) := Timeout_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info
     (Id        : Trace_T;
      Task_Name : Task_Id;
      Number    : Integer)
   is
      Task_S    : String := SSL.Task_Name.all;
      Number_S  : String := Integer'Image (Number);
      Accepts_S : String := Extract_Accepts (Task_Name);
      Trace_S   : String (1 .. 9 + Task_S'Length
                                 + Number_S'Length + Accepts_S'Length);

      L0 : Integer := Task_S'Length;
      L1 : Integer := Task_S'Length + Number_S'Length;

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3)                  := "/N:";
         Trace_S (4 .. 3 + L0)             := Task_S;
         Trace_S (4 + L0 .. 6 + L0)        := "/#:";
         Trace_S (7 + L0 .. 6 + L1)        := Number_S;
         Trace_S (7 + L1 .. 9 + L1)        := "/E:";
         Trace_S (10 + L1 .. Trace_S'Last) := Accepts_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info
     (Id        : Trace_T;
      Task_Name : Task_Id;
      Number    : Integer;
      Timeout   : Duration)
   is
      Task_S    : String := SSL.Task_Name.all;
      Timeout_S : String := Duration'Image (Timeout);
      Number_S  : String := Integer'Image (Number);
      Accepts_S : String := Extract_Accepts (Task_Name);
      Trace_S   : String (1 .. 12 + Task_S'Length + Timeout_S'Length
                                  + Number_S'Length + Accepts_S'Length);

      L0 : Integer := Task_S'Length;
      L1 : Integer := Task_S'Length + Timeout_S'Length;
      L2 : Integer := Task_S'Length + Timeout_S'Length + Number_S'Length;

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3)                  := "/N:";
         Trace_S (4 .. 3 + L0)             := Task_S;
         Trace_S (4 + L0 .. 6 + L0)        := "/T:";
         Trace_S (7 + L0 .. 6 + L1)        := Timeout_S;
         Trace_S (7 + L1 .. 9 + L1)        := "/#:";
         Trace_S (10 + L1 .. 9 + L2)       := Number_S;
         Trace_S (10 + L2 .. 12 + L2)      := "/E:";
         Trace_S (13 + L2 .. Trace_S'Last) := Accepts_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;

   ---------------------
   -- Extract_Accepts --
   ---------------------

   --  This function returns a string in which all opened
   --  Accepts or Selects are given, separated by semi-colons.

   function Extract_Accepts (Task_Name : Task_Id) return String_Trace is
      Info_Annex : String_Trace := (ASCII.NUL, others => ' ');

   begin
      for J in Task_Name.Open_Accepts'First ..
        Task_Name.Open_Accepts'Last - 1
      loop
         Info_Annex := Append (Info_Annex, Integer'Image
                               (Integer (Task_Name.Open_Accepts (J).S)) & ",");
      end loop;

      Info_Annex := Append (Info_Annex,
                            Integer'Image (Integer
                                           (Task_Name.Open_Accepts
                                            (Task_Name.Open_Accepts'Last).S)));
      return Info_Annex;
   end Extract_Accepts;
end System.Traces.Tasking;
