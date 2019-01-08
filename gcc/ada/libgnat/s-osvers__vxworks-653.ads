------------------------------------------------------------------------------
--                                                                          --
--                      GNAT RUN-TIME LIBRARY COMPONENTS                    --
--                                                                          --
--                      S Y S T E M . O S _ V E R S I O N                   --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                      Copyright (C) 2010-2019, AdaCore                    --
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
------------------------------------------------------------------------------

--  This is the VxWorks 653 Partition OS version of this file. If you add an OS
--  variant please be sure to update type OS_Version in all variants of this
--  file, which is part of the Level A certified run-time libraries.

package System.OS_Versions is
   pragma Pure (System.OS_Versions);
   type OS_Version is
     (LynxOS_178, VxWorks_Cert, VxWorks_Cert_RTP, VxWorks_653, VxWorks_MILS);
   OS : constant OS_Version := VxWorks_653;
end System.OS_Versions;
