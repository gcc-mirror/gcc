------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 ADA.TAGS.GENERIC_DISPATCHING_CONSTRUCTOR                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
--  Turn of categorization warnings

generic
   type T (<>) is abstract tagged limited private;
   type Parameters (<>) is limited private;
   with function Constructor (Params : not null access Parameters) return T
     is abstract;

function Ada.Tags.Generic_Dispatching_Constructor
  (The_Tag : Tag;
   Params  : not null access Parameters) return T'Class;
pragma Preelaborate_05 (Generic_Dispatching_Constructor);
pragma Import (Intrinsic, Generic_Dispatching_Constructor);
--  Note: the reason that we use Preelaborate_05 here is so that this will
--  compile fine during the normal build procedures. In Ada 2005 mode (which
--  is required for this package anyway), this will be treated as Preelaborate
--  so everything will be fine.
