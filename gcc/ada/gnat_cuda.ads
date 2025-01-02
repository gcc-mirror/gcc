------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 C U D A                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2010-2025, Free Software Foundation, Inc.         --
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

--  This package defines CUDA-specific datastructures and subprograms.
--
--  Compiling for CUDA requires compiling for two targets. One is the CPU (more
--  frequently named "host"), the other is the GPU (the "device"). Compiling
--  for the host requires compiling the whole program. Compiling for the device
--  only requires compiling packages that contain CUDA kernels.
--
--  When compiling for the device, GNAT-LLVM is used. It produces assembly
--  tailored to Nvidia's GPU (NVPTX). This NVPTX code is then assembled into
--  an object file by ptxas, an assembler provided by Nvidia. This object file
--  is then combined with its source code into a fat binary by a tool named
--  `fatbin`, also provided by Nvidia. The resulting fat binary is turned into
--  a regular object file by the host's linker and linked with the program that
--  executes on the host.
--
--  A CUDA kernel is a procedure marked with the CUDA_Global pragma or aspect.
--  CUDA_Global does not have any effect when compiling for the device. When
--  compiling for the host, the frontend stores procedures marked with
--  CUDA_Global in a hash table the key of which is the Node_Id of the package
--  body that contains the CUDA_Global procedure. This is done in sem_prag.adb.
--  When emitting an ALI file for a compilation unit, the frontend emits 'K'
--  lines for each visible CUDA kernel (see Output_CUDA_Symbols in
--  lib-writ.adb). This allows the binder to see all kernels in a program and
--  emit code to register the kernels with the CUDA runtime.

--  Registering a CUDA kernel with the CUDA runtime requires multiple function
--  calls:
--  - The first one registers the fat binary which corresponds to the package
--    with the CUDA runtime.
--  - Then, as many function calls as there are kernels in order to bind them
--    with the fat binary.
--    fat binary.
--  - The last call lets the CUDA runtime know that we are done initializing
--    CUDA.
--  All of that is performed by the code emitted by bindgen.adb.
--
--  Once a CUDA package is initialized, its kernels are ready to be used.
--  Launching CUDA kernels is done by using the CUDA_Execute pragma. When
--  compiling for the host, the CUDA_Execute pragma is expanded into a declare
--  block which performs calls to the CUDA runtime functions.
--  - The first one pushes a "launch configuration" on the "configuration
--    stack" of the CUDA runtime.
--  - The second call pops this call configuration, making it effective.
--  - The third call actually launches the kernel.
--  Light validation of the CUDA_Execute pragma is performed in sem_prag.adb
--  and expansion is performed in exp_prag.adb.

with Types; use Types;

package GNAT_CUDA is

   procedure Add_CUDA_Device_Entity (Pack_Id : Entity_Id; E : Entity_Id);
   --  And E to the list of CUDA_Device entities that belong to Pack_Id

   procedure Add_CUDA_Kernel (Pack_Id : Entity_Id; Kernel : Entity_Id);
   --  Add Kernel to the list of CUDA_Global nodes that belong to Pack_Id.
   --  Kernel is a procedure entity marked with CUDA_Global, Pack_Id is the
   --  entity of its parent package body.

   procedure Expand_CUDA_Package (N : Node_Id);
   --  When compiling for the host:
   --  - Empty content of CUDA_Global procedures.
   --  - Remove declarations of CUDA_Device entities.

   function Get_CUDA_Kernels (Pack_Id : Entity_Id) return Elist_Id;
   --  Returns an Elist of all procedures marked with pragma CUDA_Global that
   --  are declared within package body Pack_Body. Returns No_Elist if Pack_Id
   --  does not contain such procedures.

end GNAT_CUDA;
