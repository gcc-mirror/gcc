------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 G N A T . B O U N D E D _ M A I L B O X E S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a thread-safe asynchronous communication facility
--  in the form of mailboxes. Individual mailbox objects are bounded in size
--  to a value specified by their Capacity discriminants.

--  Mailboxes actually hold references to messages, not the message values
--  themselves.

--  Type Mailbox is defined explicitly as a protected type (via derivation
--  from a protected type) so that clients may treat them accordingly (for
--  example, by making conditional/timed entry calls).

with System;
with GNAT.Bounded_Buffers;

generic
   type Message (<>) is limited private;
   type Message_Reference is access all Message;
   --  Mailboxes hold references to Message values, of this type

package GNAT.Bounded_Mailboxes is
   pragma Preelaborate;

   package Message_Refs is
      new GNAT.Bounded_Buffers (Message_Reference);

   type Mailbox is new Message_Refs.Bounded_Buffer;

   --  Type Mailbox has two inherited discriminants:

   --  Capacity : Positive;
   --     Capacity is the maximum number of Message references
   --     possibly contained at any given instant.

   --  Ceiling : System.Priority;
   --     Users must specify the ceiling priority for the object.
   --     If the Real-Time Systems Annex is not in use this value
   --     is not important.

   --  Protected type Mailbox has the following inherited interface:

   --  entry Insert (Item : Message_Reference);
   --     Insert Item into the Mailbox. Blocks caller
   --     until space is available.

   --  entry Remove (Item : out Message_Reference);
   --     Remove next available Message_Reference from Mailbox.
   --     Blocks caller until a Message_Reference is available.

   --  function Empty return Boolean;
   --     Returns whether the Mailbox contains any Message_References.
   --     Note: State may change immediately after call returns.

   --  function Full return Boolean;
   --     Returns whether any space remains within the Mailbox.
   --     Note: State may change immediately after call returns.

   --  function Extent return Natural;
   --     Returns the number of Message_Reference values currently held
   --     within the Mailbox.
   --     Note: State may change immediately after call returns.

   Default_Ceiling : constant System.Priority := Message_Refs.Default_Ceiling;
   --  A convenience value for the Ceiling discriminant

end GNAT.Bounded_Mailboxes;
