------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          S Y S T E M . S O F T _ L I N K S . I N I T I A L I Z E         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2017-2023, Free Software Foundation, Inc.       --
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

--  This package exists to initialize the TSD record of the main task and in
--  the process, allocate and initialize the secondary stack for the main task.
--  The initialization routine is contained within its own package because
--  System.Soft_Links and System.Secondary_Stack are both Preelaborate packages
--  that are the parents to other Preelaborate System packages.

--  Ideally, the secondary stack would be set up via __gnat_runtime_initialize
--  to have the secondary stack active as early as possible and to remove the
--  awkwardness of System.Soft_Links depending on a non-Preelaborate package.
--  However, as this procedure only exists from 2014, for bootstrapping
--  purposes the elaboration mechanism is used instead to perform these
--  functions.

package System.Soft_Links.Initialize is
   pragma Elaborate_Body;
   --  Allow this package to have a body
end System.Soft_Links.Initialize;
