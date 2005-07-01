------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                               D E C . I O                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This is an AlphaVMS package that contains the declarations and
--  function specifications needed by the DECLib IO packages.

with System.Task_Primitives;
package DEC.IO is
private

   type Exception_Number is (
     GNAT_EN_LOCK_ERROR,
     GNAT_EN_EXISTENCE_ERROR,
     GNAT_EN_KEY_ERROR,
     GNAT_EN_KEYSIZERR,
     GNAT_EN_STAOVF,
     GNAT_EN_CONSTRAINT_ERRO,
     GNAT_EN_IOSYSFAILED,
     GNAT_EN_LAYOUT_ERROR,
     GNAT_EN_STORAGE_ERROR,
     GNAT_EN_DATA_ERROR,
     GNAT_EN_DEVICE_ERROR,
     GNAT_EN_END_ERROR,
     GNAT_EN_MODE_ERROR,
     GNAT_EN_NAME_ERROR,
     GNAT_EN_STATUS_ERROR,
     GNAT_EN_NOT_OPEN,
     GNAT_EN_ALREADY_OPEN,
     GNAT_EN_USE_ERROR,
     GNAT_EN_UNSUPPORTED,
     GNAT_EN_FAC_MODE_MISMAT,
     GNAT_EN_ORG_MISMATCH,
     GNAT_EN_RFM_MISMATCH,
     GNAT_EN_RAT_MISMATCH,
     GNAT_EN_MRS_MISMATCH,
     GNAT_EN_MRN_MISMATCH,
     GNAT_EN_KEY_MISMATCH,
     GNAT_EN_MAXLINEXC,
     GNAT_EN_LINEXCMRS);

   for Exception_Number'Size use 32;

   for Exception_Number use (
     GNAT_EN_LOCK_ERROR => 1,
     GNAT_EN_EXISTENCE_ERROR => 2,
     GNAT_EN_KEY_ERROR => 3,
     GNAT_EN_KEYSIZERR => 4,
     GNAT_EN_STAOVF => 5,
     GNAT_EN_CONSTRAINT_ERRO => 6,
     GNAT_EN_IOSYSFAILED => 7,
     GNAT_EN_LAYOUT_ERROR => 8,
     GNAT_EN_STORAGE_ERROR => 9,
     GNAT_EN_DATA_ERROR => 10,
     GNAT_EN_DEVICE_ERROR => 11,
     GNAT_EN_END_ERROR => 12,
     GNAT_EN_MODE_ERROR => 13,
     GNAT_EN_NAME_ERROR => 14,
     GNAT_EN_STATUS_ERROR => 15,
     GNAT_EN_NOT_OPEN => 16,
     GNAT_EN_ALREADY_OPEN => 17,
     GNAT_EN_USE_ERROR => 18,
     GNAT_EN_UNSUPPORTED => 19,
     GNAT_EN_FAC_MODE_MISMAT => 20,
     GNAT_EN_ORG_MISMATCH => 21,
     GNAT_EN_RFM_MISMATCH => 22,
     GNAT_EN_RAT_MISMATCH => 23,
     GNAT_EN_MRS_MISMATCH => 24,
     GNAT_EN_MRN_MISMATCH => 25,
     GNAT_EN_KEY_MISMATCH => 26,
     GNAT_EN_MAXLINEXC => 27,
     GNAT_EN_LINEXCMRS => 28);

   procedure Raise_IO_Exception (EN : Exception_Number);
   pragma Export_Procedure (Raise_IO_Exception, "GNAT$RAISE_IO_EXCEPTION",
      Mechanism => Value);

   package IO_Locking is
      type Access_Mutex is private;
      function Create_Mutex return Access_Mutex;
      procedure Acquire (M : Access_Mutex);
      procedure Release (M : Access_Mutex);

   private
      type Access_Mutex is access System.Task_Primitives.RTS_Lock;
      pragma Export_Function (Create_Mutex, "GNAT$CREATE_MUTEX",
          Mechanism => Value);
      pragma Export_Procedure (Acquire, "GNAT$ACQUIRE_MUTEX",
          Mechanism => Value);
      pragma Export_Procedure (Release, "GNAT$RELEASE_MUTEX",
          Mechanism => Value);
   end IO_Locking;

end DEC.IO;
