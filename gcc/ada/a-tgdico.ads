------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 ADA.TAGS.GENERIC_DISPATCHING_CONSTRUCTOR                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

generic
   type T (<>) is abstract tagged limited private;
   type Parameters (<>) is limited private;
   with function Constructor (Params : access Parameters) return T is abstract;

function Ada.Tags.Generic_Dispatching_Constructor
  (The_Tag : Tag; Params : access Parameters) return T'Class;

--  pragma Preelaborate (Generic_Dispatching_Constructor);
--  Commented out temporarily because various other predefined units do not
--  yet have proper categorization as specified by AI-362 (such as Ada.Tags,
--  Ada.Exceptions, etc.).

pragma Import (Intrinsic, Generic_Dispatching_Constructor);
